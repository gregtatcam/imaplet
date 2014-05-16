(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Core.Std
open Async.Std
open Async_unix
open Configuration
open Utils
open Regex
open Storage
open StorageMeta
open IrminStorageClnt
open Mflags
open Email_message

exception BadMailbox of string
exception TruncatedMessage
exception FailedStatus

type selection = Select of string | Examine of string

type t = string * string * string option * selection option * 
  (Reader.t * Writer.t) option

(** locks TBD !!!! **)

(** inbox folder, mailboxes folder, user account, selected -> type **)
(** validate directory and inbox TBD **)
let create user str_rw =
  let mbox =
    if (Configuration.get_store()) = `Irminsule then
      Configuration.irmin_mailboxes ()
    else
      Configuration.mailboxes user
  in
  ((inbox user), mbox, Some user, None, str_rw)

let update mbx s : t=
  let (i,m,u,_,rw) = mbx in
  (i,m,u,s,rw)

let selected_mbox mbx =
  let (_,_,_,s,rw) = mbx in
  match s with
  | None -> None
  | Some s ->
      match s with 
      | Examine n -> Some n
      | Select n -> Some n

(** create an empty amailbox type **)
let empty () = ("","",None,None,None)

(** is empty type **)
let is_empty mbx =
  let (_, _, u,_,_) = mbx in
  match u with 
  | None -> true
  | Some _ -> false

(** make directory item **)
let dir_item item cnt =
  (item, "\\Noselect" ::
    if cnt = 0 then
      ["\\HasNoChildren"]
    else
      ["\\HasChildren"])

(** make mailbox item **)
let mbox_item item reference =
  (item, "NoInferiors" ::
  if reference = "" then 
  (
    if item = "Drafts" then
      ["\\Drafts"]
    else if item = "Deleted Messages" then
      ["\\Deleted"]
    else if item = "Sent Messages" then
      ["\\Sent"] 
    else
      []
  ) else
    []
  )

(** get mailbox path **)
let get_mbox_path mbx name =
  let (i,m,_,_,_) = mbx in
  (if Configuration.get_store() <> `Irminsule && match_regex ~case:false name "INBOX" = true then
    i
  else
    concat_path m name)

let storage_factory_mbox mbx mailbox1 ?mailbox2 () =
  let mk_arg mbx mailbox =
    let file = get_mbox_path mbx mailbox in
    let open Primitives in
    let (_,m,_,_,_) = mbx in
    let loc = BasicLocation.create file in
    let mloc = BasicLocation.create m in
    let iloc = BasicLocation.create (Configuration.inbox_root()) in
    (loc, mloc, iloc, Configuration.mbox_index_params())
  in
  let tp1 = mk_arg mbx mailbox1 in
  let tp2 = if mailbox2 = None then None else Some (mk_arg mbx (Option.value_exn mailbox2)) in
  build_strg_inst (module UnixMboxMailboxStorage) tp1 ?tp2 ()

let storage_factory_irmin mbx mailbox1 ?mailbox2 () =
  let mk_arg mbx mailbox =
    let file = get_mbox_path mbx mailbox in
    let open Primitives in
    let (_,m,u,_,rw) = mbx in
    let loc = BasicLocation.create file in
    let mloc = BasicLocation.create m in
    let iloc = BasicLocation.create (Configuration.irmin_inbox_root()) in
    let (r,w) = Option.value_exn rw in
    let param = (Option.value_exn u,r,w) in
    (loc, mloc, iloc, param)
  in
  let tp1 = mk_arg mbx mailbox1 in
  let tp2 = if mailbox2 = None then None else Some (mk_arg mbx (Option.value_exn mailbox2)) in
  build_strg_inst (module IrminsuleStorage) tp1 ?tp2 ()

let storage_factory mbx mailbox1 ?mailbox2() =
  match (Configuration.get_store()) with
  | `Irminsule -> storage_factory_irmin mbx mailbox1 ?mailbox2 ()
  | _ -> storage_factory_mbox mbx mailbox1 ?mailbox2 ()

let mailbox_exists mbx mailbox =
  let (module Mailbox) = storage_factory mbx mailbox () in
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
    | `No -> return false
    | `Folder -> return true
    | `Storage -> return true

(** check if the mailbox exists and is not a directory **)
let valid_mailbox mbx mailbox : ([`NotExists|`NotSelectable|`ValidMailbox] Deferred.t)=
  let (module Mailbox) = storage_factory mbx mailbox () in
  Mailbox.MailboxStorage.exists Mailbox.this >>= function
    | `No -> return `NotExists
    | `Folder -> return `NotSelectable
    | `Storage -> return `ValidMailbox

(** get mailbox path from index path **)
let get_mbox_from_index mbx index : (string) =
  let (i,m,_,_,_,_) = mbx in
  let l = Str.split (Str.regexp "/\\.imaplet/") index in
  if m = List.nth_exn l 0 && match_regex ~case:false (List.nth_exn l 1) "inbox" then
    i
  else
    concat_path (List.nth_exn l 0) (List.nth_exn l 1)

let get_mailbox_metadata mbx mailbox =
  let (module Mailbox) = storage_factory mbx mailbox () in
  Mailbox.MailboxStorage.get_mailbox_metadata Mailbox.this

(** select a mailbox 
 * assuming that if index file exists it must be up-to-date
 * need to redo Invalid/Directory/etc TBD
**)
let select_ mbx name (read_only:bool) : ([`NotExists|`NotSelectable|`Error of string|`Ok of t * mailbox_metadata] Deferred.t) =
  printf "select_\n%!";
  valid_mailbox mbx name >>= function
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `ValidMailbox -> 
    get_mailbox_metadata mbx name >>= fun header ->
      let sel =
        if read_only = true then
          Some (Select name)
        else
          Some (Examine name)
      in
      return (`Ok((update mbx sel),header))

(** select mailbox **)
let select mbx name : 
  ([`NotExists|`NotSelectable|`Error of string|`Ok of t * mailbox_metadata] Deferred.t) =
  select_ mbx name false

(** examine mailbox, same as select but read-only **)
let examine mbx name : 
  ([`NotExists|`NotSelectable|`Error of string|`Ok of t * mailbox_metadata] Deferred.t) =
  select_ mbx name true

(** close mailbox **)
let close mbx =
  (update mbx None)
  
(** email root folder -> relative root -> maillbox [wildcards] -> recursive
 * relative path from the relative root -> list of mailboxes/flags 
 * recursively list directory content **)
let rec listmbx_ mailbxs (reference:string) (mailbox:string)
(filter:('a,'b,'comp) Map.t) : (string *string list) list Deferred.t =
  (* item is relative to the reference *)
  let select_item acc reference item isdir =
    if match_dot item then 
      (acc)
    else
    (
      (** fix regex for the mailbox **)
      let regxmailbox = fixregx_mbox mailbox in
      (** get item path relative to the relative root **)
      let filtered = (Map.is_empty filter) = false &&
        (match Map.find filter item with None -> true| Some _ -> false) in
      let tomatch = concat_path reference item in
      if isdir <> None then
      (
        if match_regex tomatch regxmailbox && filtered = false then
          ((dir_item item (Option.value_exn isdir)) :: acc)
        else
          acc
      ) else if match_regex tomatch regxmailbox && filtered = false then
          ((mbox_item item reference) :: acc)
        else
          (acc)
    )
  in
  (** get mailboxes root **)
  let (module Mailbox) = storage_factory mailbxs reference () in
  Mailbox.MailboxStorage.exists Mailbox.this >>= fun res ->
  if res <> `Folder then
    return ([])
  else
    (* item has path relative to the start, i.e. root + reference *)
    Mailbox.MailboxStorage.list_store Mailbox.this ~init:[] ~f:(fun acc item ->
      match item with
        | `Folder (item,cnt)  -> return (select_item acc reference item (Some cnt))
        | `Storage item  -> return (select_item acc reference item None)
    )

(** add to the calculated list the reference folder and inbox **)
let add_list reference mailbox acc =
  let fixed = fixregx_mbox mailbox in
  if match_regex reference "[.]+" = false && match_regex "INBOX" fixed then 
    ("INBOX", ["\\HasNoChildren"])::acc
  else
    acc

(** list mailbox **)
let listmbx_filt mailbx (reference:string) (mailbox:string) (filter:('a,'b,'comp) Map.t) : 
  (string*string list) list Deferred.t =
  printf "listmbx -%s- -%s-\n%!" reference mailbox;
  let fixref = replace "\"" "" reference in 
  let fixref = replace "^/$" "" fixref in
  let fixmbx = replace "\"" "" mailbox in
  if reference = "\"/\"" || reference = "/" || reference = "" && mailbox = "" then
  (
    printf "special listmbx -%s- -%s-\n%!" reference mailbox;
    let flags = ["\\Noselect"] in
    let file = 
    (if mailbox = "" then
      "/"
    else
      ""
    ) in
      return ([file, flags])
  ) else (
    printf "regular listmbx -%s- -%s-\n%!" reference mailbox;
    listmbx_ mailbx fixref fixmbx filter >>= 
      fun acc -> return (add_list fixref mailbox acc)
  )

(** list mailbox **)
let listmbx mbx (reference:string) (mailbox:string) : 
  (string*string list) list Deferred.t = 
  listmbx_filt mbx reference mailbox (Map.empty ~comparator:String.comparator)

(** lsubmbx mailbx reference mailbox, list on subscribed mailboxes 
 * need to handle a wild card % case when foo/bar is subscribed but foo is no
 * should return foo only in this case 
**)
let lsubmbx mbx (reference:string) (mailbox:string) : (string*string list) list Deferred.t = 
  printf "lsubmbx\n%!";
  let (module Mailbox) = storage_factory mbx "" () in
  Mailbox.MailboxStorage.get_subscription Mailbox.this >>= fun l -> 
    let filt = List.fold l ~init:String.Map.empty ~f:(fun acc i -> String.Map.add acc ~key:i ~data:0)
    in
    listmbx_filt mbx reference mailbox filt

(** create mailbox **)
let create_mailbox mbx (mailbox:string) : [`Error of string|`Ok] Deferred.t =
  valid_mailbox mbx mailbox >>= function
    | `ValidMailbox | `NotSelectable -> return (`Error("Mailbox already exists"))
    | `NotSelectable -> return (`Error("Invalid Superior"))
    | `NotExists ->
      if match_regex mailbox "^/" then
        return (`Error("Invalid mailbox name: Begins with hierarchy separator"))
      else if match_regex mailbox "^\"?.imaplet/?\"?" then
        return (`Error("Invalid mailbox name: Contains reserved name"))
      else if match_regex mailbox "^\"?./?\"?$" || match_regex mailbox "^\"?../?\"?$" then
        return (`Error("Invalid mailbox name: Contains . part"))
      else 
      (
        let (module Mailbox) = storage_factory mbx mailbox () in
        Mailbox.MailboxStorage.create Mailbox.this >>= fun _ -> return `Ok
      )

(** delete mailbox **)
let delete_mailbox mbx mailbox : [`Error of string|`Ok] Deferred.t =
  mailbox_exists mbx mailbox >>= fun e ->
  if e = false then 
    return (`Error("Mailbox doesn't exist"))
  else
    let (module Mailbox) = storage_factory mbx mailbox () in
    Mailbox.MailboxStorage.delete Mailbox.this >>= fun _ -> return `Ok

(** rename a mailbox **)
let rename_mailbox mbx (src:string) (dst:string) : 
  [`Error of string|`Ok] Deferred.t =
  mailbox_exists mbx src >>= fun e ->
  if e = false then 
    return (`Error("Mailbox doesn't exist"))
  else
    let (module Mailbox) = storage_factory mbx src ~mailbox2:dst () in
    Mailbox.MailboxStorage.move Mailbox.this (Option.value_exn Mailbox.this1) >>= fun _ -> return `Ok

(** subscribe a mailbox **)
let subscribe mbx (mailbox:string) : [`Ok|`Error of string] Deferred.t =
  mailbox_exists mbx mailbox >>= fun e ->
  if e = false then 
    return (`Error("Mailbox doesn't exist"))
  else
    let (module Mailbox) = storage_factory mbx mailbox () in
    Mailbox.MailboxStorage.subscribe Mailbox.this >>= fun () -> return `Ok

(** unsubscribe a mailbox **)
let unsubscribe mbx (mailbox:string) : [`Error of string|`Ok] Deferred.t =
  let (module Mailbox) = storage_factory mbx mailbox () in
  Mailbox.MailboxStorage.unsubscribe Mailbox.this >>= fun () -> return `Ok

(** get status *)
let get_status mbx mailbox optlist :
  ([`NotExists|`NotSelectable|`Error of string|`Ok of mailbox_metadata] Deferred.t) =
  valid_mailbox mbx mailbox >>= function
    | `NotExists -> return `NotExists
    | `NotSelectable -> return `NotSelectable
    | `ValidMailbox -> get_mailbox_metadata mbx mailbox >>= fun m -> return (`Ok m)

(** append to the mailbox, read from the network more data per literal
 * write to the client before reading unles + literal 
 * the data is parsed into new mailbox structure for further processing
 * if the mailbox consists of only the template header message then this message
 * should be overwritten! TBD
 **)
let append mbx (name:string) (reader:Reader.t) (writer:Writer.t) (flags:mailboxFlags list option)
  (date:Time.t option) (literal:States.literal) :
    ([`NotExists|`NotSelectable|`Eof of int|`Error of string|`Ok] Deferred.t) =
  let dt = Time.to_string (Option.value date ~default:Time.epoch) in
  printf "append %s %s %d\n%!" name dt (List.length (Option.value flags ~default:[]));
  States.pr_flags flags;
  valid_mailbox mbx name >>= function
    | `NotExists -> return (`NotExists)
    | `NotSelectable -> return (`NotSelectable)
    | `ValidMailbox -> 
      (** request the message from the client **)
      let size = (match literal with
      | States.Literal n -> Response.write_resp writer (Resp_Cont("")); n
      | States.LiteralPlus n -> n) in
        let (module Mailbox) = storage_factory mbx name () in
        Mailbox.MailboxStorage.fold Mailbox.this ~exclusive:true ~init:() ~f:(fun () accs ->
          (* read from the net and write to the email parser *)
          (****** temp, use With_pipe.of_string to construct the message until
           * Email_message is updated in git
          let pipe_read = Pipe.init (fun pipe_write ->
            let rec read size =
              if size = 0 then (
                Pipe.close pipe_write; return()
              ) else (
                printf "append ------------- %d\n%!" size;
                let buff_size = if size > 1024 then 1024 else size in
                let buff = String.create buff_size in
                Reader.really_read reader buff >>= function
                 | `Ok -> Pipe.write pipe_write buff >>= fun () -> read (size - buff_size)
                 | `Eof i -> 
                    if i = 0 then
                      return ()
                    else (
                      Pipe.write pipe_write (String.slice buff 0 i) >>= fun () -> 
                       Pipe.close pipe_write; return ()
                    )
              )
            in
            Pipe.write pipe_write "From dovecot@localhost.local  Thu Mar 20 17:23:22 2014\r\n" >>= fun () ->
            read size
          ) in 
          ******)
          (* need to handle bad message TBD *)
          let rec read buffer size =
            let buff_size = if size > 1024 then 1024 else size in
            let buff = String.create buff_size in
            Reader.really_read reader buff >>= function
            | `Ok -> Buffer.add_string buffer buff; read buffer (size - buff_size)
            | `Eof i -> 
              if i = 0 then
                return buffer 
              else (
                Buffer.add_string buffer (String.slice buff 0 i);
                return buffer 
              )
          in
          read (Buffer.create size) size >>= fun buffer ->
          let open Email_message in
          (******
          Mailbox.With_pipe.t_of_pipe pipe_read >>= fun mailbox ->
          ******)
          Mailbox.With_pipe.of_string (Buffer.contents buffer) >>= fun mailbox ->
          (* fold over structured email messages *)
          Pipe.fold mailbox 
          ~init:()
          ~f:(fun () message -> 
            let (module Accessor : StorageAccessor_inst) = accs in
            (* need to add flags, TBD *)
            let flags = Option.value flags ~default:[] in
            let flags =
            if List.find flags ~f:(fun f -> if f = Flags_Recent then true else false) <> None then
              Some flags
            else
              Some (Flags_Recent :: flags)
            in
            let metadata = empty_mailbox_message_metadata() in
            let metadata = update_mailbox_message_metadata ~data:metadata
            ?internal_date:date ?flags ()
            in
            Accessor.StorageAccessor.writer Accessor.this `Append
            (message,metadata) >>= fun _ -> return ()
          )
        ) >>= fun () -> return `Ok

(** find messages matching the search criteria
**)
let search mbx (keys:('a)States.searchKeys) (buid:bool) : 
  [`NotExists|`NotSelectable|`Error of string|`Ok of int list] Deferred.t =
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
  valid_mailbox mbx name >>= function
    | `NotExists -> return (`NotExists)
    | `NotSelectable -> return (`NotSelectable)
    | `ValidMailbox -> 
      let (module Mailbox) = storage_factory mbx name () in
      Mailbox.MailboxStorage.search_with Mailbox.this ~filter:(buid,keys) >>= fun acc -> return (`Ok acc)

let fetch (mbx:t) (resp_writer:(string->unit)) (sequence:States.sequence) (fetchattr:States.fetch)
(buid:bool) : [`NotExists|`NotSelectable|`Error of string|`Ok ] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
  valid_mailbox mbx name >>= function
    | `NotExists -> return (`NotExists)
    | `NotSelectable -> return (`NotSelectable)
    | `ValidMailbox -> 
      let (module Mailbox) = storage_factory mbx name () in
      Mailbox.MailboxStorage.fold Mailbox.this ~exclusive:true ~init:() ~f:(fun () accs ->
        let rec doread accs seq =
          let (module Accessor : StorageAccessor_inst) = accs in
          let filter = Some (States.Key (States.Search_SeqSet sequence)) in
          Accessor.StorageAccessor.reader Accessor.this ?filter (`Position seq) >>= function
            | `Eof -> return ()
            | `NotFound -> doread accs (seq + 1)
            | `Ok (message,metadata) -> 
              printf "============= %s" (Email_message.Mailbox.Message.to_string message);
              let res = Interpreter.exec_fetch seq sequence message metadata fetchattr buid in
              match res with
              | Some res -> resp_writer res; doread accs (seq + 1)
              | None -> doread accs (seq + 1)
        in
        doread accs 1
      ) >>= fun () -> return `Ok

let store (mbx:t) (resp_writer:(string->unit)) (sequence:States.sequence)
(storeattr:States.storeFlags) (flagsval:mailboxFlags list)
(buid:bool) : [`NotExists|`NotSelectable|`Error of string|`Ok] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
  valid_mailbox mbx name >>= function
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `ValidMailbox ->
      let (module Mailbox) = storage_factory mbx name () in
      Mailbox.MailboxStorage.fold Mailbox.this ~exclusive:true ~init:() ~f:(fun () accs ->
        let rec doread accs seq =
          let (module Accessor : StorageAccessor_inst) = accs in
          Accessor.StorageAccessor.reader_metadata Accessor.this (`Position seq) >>= function
            | `Eof -> return ()
            | `Ok metadata -> 
              let update metadata seq =
                Accessor.StorageAccessor.writer_metadata Accessor.this metadata 
                (`Position seq) >>= function
                 | `Eof -> return ()
                 | `Ok -> doread accs (seq + 1)
              in
              match Interpreter.exec_store metadata seq sequence storeattr flagsval buid with
              | `None -> doread accs (seq + 1)
              | `Silent metadata -> update metadata seq
              | `Ok (metadata,res) -> resp_writer res;update metadata seq
        in
        doread accs 1
      ) >>= fun () -> return `Ok

(** if copy fails it should restore the mailbox, so need to copy TBD **)
let copy (mbx:t) (dest_mbox:string) (sequence:States.sequence) (buid:bool) :
[`NotExists|`NotSelectable|`Error of string|`Ok] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
    valid_mailbox mbx dest_mbox >>= function
    | `NotSelectable -> return `NotSelectable
    | `NotExists -> return `NotExists
    | `ValidMailbox -> 
      let (module Mailbox) = storage_factory mbx name ~mailbox2:dest_mbox () in
      Mailbox.MailboxStorage.copy_with Mailbox.this (Option.value_exn Mailbox.this1) 
      ~filter:(buid, sequence) >>= fun _ -> return `Ok

(** permanently remove messages with \Deleted flag set 
 * create a temp mailbox, copy filtered content to the box
 * need to revert if fails TBD
**)
let expunge (mbx:t) (resp_writer:(string->unit)) : 
  [`Error of string|`Ok] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
    let tmp_mbox = ".imaplet." ^ new_uidvalidity() in
    let (module Mailbox) = storage_factory mbx name ~mailbox2:tmp_mbox () in
    Mailbox.MailboxStorage.expunge Mailbox.this ~tmp:(Option.value_exn Mailbox.this1) 
    ~f:(fun seq -> resp_writer ((string_of_int seq) ^ " EXPUNGE")) >>= fun _ ->
      return `Ok
