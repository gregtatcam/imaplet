open Core.Std
open Async.Std
open Async_unix
open Configuration
open Utils
open Regex
open Email_message
open Index

exception BadMailbox of string
exception TruncatedMessage
exception FailedStatus

type selection = Select of string | Examine of string

type t = string * string * string option * selection option

(** locks TBD !!!! **)

(** inbox folder, mailboxes folder, user account, selected -> type **)
(** validate directory and inbox TBD **)
let create user =
  ((inbox user), (mailboxes user), Some user, None)

let update (mbx:t) (s:selection option) : t=
  let (i,m,u,_) = mbx in
  (i,m,u,s)

let selected_mbox mbx =
  let (_,_,_,s) = mbx in
  match s with
  | None -> None
  | Some s ->
      match s with 
      | Examine n -> Some n
      | Select n -> Some n

(** create an empty amailbox type **)
let empty () = ("","",None,None)

(** is empty type **)
let is_empty mbx =
  let (_, _, u,_) = mbx in
  match u with 
  | None -> true
  | Some _ -> false

(** make directory item **)
let dir_item item l =
  (item, "\\Noselect" ::
  (match l with 
  |[] -> ["\\HasNoChildren"]
  |_ -> ["\\HasChildren"]))

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
  let (i,m,_,_) = mbx in
  (if match_regex ~case:false name "INBOX" = true then
    i
  else
    concat_path m name)

(** index file is in the mailbox's .imaplet folder **)
let get_index_path mbx name =
  let (i,m,_,_) = mbx in
  let file = concat_path m name in
  let folder = Filename.dirname file in
  let mbox = Filename.basename file in
  make_path [folder;".imaplet";mbox]

(** get subscription path **)
let get_subscr_path mbx =
  let (i,m,_,_) = mbx in
  concat_path m ".subscriptions"

(** accumulate line read **)
let rec read_lines r acc cb =
  Reader.read_line r >>= function
  | `Eof -> return (acc)
  | `Ok line -> let acc = cb acc line in read_lines r acc cb

(** fold file over the lines **)
let file_fold file ~acc ~cb =
  try_with(fun() ->
    Reader.with_file file ~exclusive:true
    ~f:(fun r -> read_lines r acc cb)
  ) >>= function 
    | Ok res -> return(res)
    | Error e -> return(acc) (** maybe should have another fold with exception?
    TBD **)

(** check if the mailbox exists and is not a directory **)
let valid_mailbox mbx mailbox : ([`NotExists|`NotSelectable|`ValidMailbox] Deferred.t)=
  let file = get_mbox_path mbx mailbox in
  file_exists file >>= fun e ->
    if e = false then
      return (`NotExists)
    else
    (
      is_directory file >>= fun e ->
      if e = true then
        return (`NotSelectable)
      else
        return (`ValidMailbox)
    )

let date_time_1 time =
  let tm = Unix.gmtime time in
  Printf.sprintf "%s %s %d %02d:%02d:%02d %d" (day_of_week tm.tm_wday) (int_to_month
  tm.tm_mon) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec (1900+tm.tm_year) 

let date_time_2 time =
  let tm = Unix.gmtime time in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d" (day_of_week tm.tm_wday)
  tm.tm_mday (int_to_month tm.tm_mon) (1900+tm.tm_year) tm.tm_hour tm.tm_min tm.tm_sec 

(** create empty mailbox with boiler-plate message **)
let init_mailbox mbx name (header:mbox_header) : (unit Deferred.t) =
  let file = get_mbox_path mbx name in
    Writer.with_file file ~exclusive:true
    ~f:(fun w -> 
      let text = "From MAILER_DAEMON  DATE1
Date: DATE2 
From: Mail System Internal Data <MAILER-DAEMON@MACHINE>
Subject: DON'T DELETE THIS MESSAGE -- FOLDER INTERNAL DATA
Message-ID: <TIME@MACHINE>
X-IMAP: UIDVALIDITY UIDNEXT $NotJunk $Junk NotJunk                                          
Status: RO

This text is part of the internal format of your mail folder, and is not
a real message.  It is created automatically by the mail system software.
If deleted, important folder data will be lost, and it will be re-created
with the data reset to initial values.

" in 
      let tm = Unix.time() in
      let date1 = date_time_1 tm in
      let date2 = date_time_2 tm in
      let machine = Unix.gethostname() in
      let time = string_of_int (Float.to_int tm) in
      let text = replace "DATE1" date1 text in
      let text = replace "DATE2" date2 text in
      let text = replace "MACHINE" machine text in
      let text = replace "TIME" time text in
      let text = replace "UIDVALIDITY" header.uidvalidity text in
      let text = replace "UIDNEXT" (Printf.sprintf "%010d" header.uidnext) text in
      Writer.write_line w text; Writer.flushed w
    ) 

(** fold the mailbox with the given acc and function cb, the cb takes
 * mailbox object, acc, and the message **)
let mbox_fold mbx name ~(acc:'a) ~(cb:('b -> 'a -> Mailbox.Message.t -> 'a Deferred.t)) : 
  ([>`NotExists|`NotSelectable|`Ok of 'a] Deferred.t) =
  valid_mailbox mbx name >>= function
  | `NotExists -> return (`NotExists)
  | `NotSelectable -> return (`NotSelectable)
  | `ValidMailbox ->
  let file = get_mbox_path mbx name in
    Email_message.Mailbox.With_pipe.t_of_file file >>= fun mailbox ->
    Pipe.fold mailbox
    ~init:(acc)
    ~f:(fun acc message -> cb mailbox acc message) >>= fun data -> return
    (`Ok (data))

(** get mailbox path from index path **)
let get_mbox_from_index mbx index : (string) =
  let (i,m,_,_) = mbx in
  let l = Str.split (Str.regexp "/\\.imaplet/") index in
  if m = List.nth_exn l 0 && match_regex ~case:false (List.nth_exn l 1) "inbox" then
    i
  else
    concat_path (List.nth_exn l 0) (List.nth_exn l 1)

(** this is only called to rebuild the index file
 * - need to accumulate overal statistics for the index header
 * - for the given email message collect the data and write it to the
 * - index file
 * **)
let rebuild_index mbx name : ((mbox_header,string) Result.t Deferred.t) =
  printf "rebuild_index\n%!";
  let file = get_index_path mbx name in
  Index.write_with_file_on_cb file false (** write record to index **)
  (fun write_record_to_index ->
    mbox_fold mbx name (** iterate over the mailbox messages **)
    ~acc:(Index.empty_mbox_header()) 
    ~cb:(fun _ index_header message -> 
      let email_headers = Header.to_list (Email.header message.email) in 
      let index_header = ({index_header with count = (index_header.count + 1)}) in
      let (index_header, record, stat, counted) =
      List.fold email_headers
      ~init:(index_header,Index.empty_mbox_msg_metadata(),false,true) (** true
      if the status header is found , true if the message should be counted **)
      ~f:(fun (index_header, record, stat, counted) (name,value) ->
        printf "rebuild_index %s %s\n" name value;
        let value = replace "^[ ]+" "" value in
        if name = "From" && match_regex value "Mail System Internal Data" then (
          printf "internal data\n%!";
          (index_header, record, stat, false)
        ) else if (name = "X-IMAPbase" || name = "X-IMAP") &&
          match_regex value "^[ ]*\\([^ ]+\\) \\([0-9]+\\)" then
        (
          let index_header = ({index_header with uidvalidity = Str.matched_group 1 value;
            uidnext = 1 + int_of_string (Str.matched_group 2 value)}) in
          printf "rebuild_index %s\n%!" index_header.uidvalidity;
            (index_header, record, stat,counted)
        ) 
        else if name = "Status" then
        (
          (** R - \Seen flag, O - non-\Recent flag **)
          let r = match_regex value "[R]" in
          let o = match_regex value "[O]" in
          let nunseen = (if r = false then (index_header.nunseen+1) else index_header.nunseen) in
          let recent = (if o = true then index_header.recent else index_header.recent+1) in
          let index_header = ({index_header with unseen = index_header.count;recent;nunseen}) in
          let record = ( 
          if r = true then
            {record with flags = "\\Seen" :: record.flags}
          else
            record
          ) in
          if o = false && index_header.unseen = 0 then 
            (index_header, record, true,counted)
          else
          (
            let index_header = {index_header with nunseen;recent} in
            (index_header, record,true,counted)
          )
        ) else if name = "X-UID" then (
          let record = {record with uid = int_of_string value} in
          (index_header, record, stat,counted)
        ) else if name = "Date" then (
          let record = {record with internal_date = try email_to_date_time_exn value with _ -> Time.now() } in
          (index_header, record, stat,counted)
        ) else
          (index_header, record, stat,counted)
      ) in
        let index_header =
        if counted then
          index_header
        else
          {index_header with count = index_header.count - 1}
        in
        let index_header =
        (if stat = false && index_header.unseen = 0 then
        (
          {index_header with unseen = index_header.count}
        ) else
          index_header
        ) in 
        let (index_header,uid) = (** keep uid from the email if it looks sane, otherwise
          re-assign, also need to write it back to the mailbox but email_message
          doesn't suport write TBD **)
        if record.uid >= 0 && record.uid < index_header.uidnext then
          (index_header,record.uid)
        else if record.uid = index_header.uidnext then
          let uidnext = index_header.uidnext + 1 in
          ({index_header with uidnext}, record.uid)
        else 
          let uidnext = index_header.uidnext + 1 in
          ({index_header with uidnext}, uidnext)
        in
        let size = (match (Email.raw_content message.email) with
        | None -> 0
        | Some cont -> Octet_stream.length cont
        ) in 
        let record = {record with uid;size} in
        write_record_to_index record >>= fun () -> return (index_header)
    ) >>= function
      | `NotExists -> return (Error ("Mailbox doesn't exist"))
      | `NotSelectable -> return (Error ("Mailbox is not selectable"))
      | `Ok header -> return (Ok header)
  )

(** initialize index file if doesn't exist **)
let get_index_with_init ?(with_mbox=false) mbx name : (mbox_header,string) Result.t Deferred.t =
  printf "get_index_with_init\n%!";
  let file = get_index_path mbx name in
  file_exists file >>= fun e ->
  (** if exists need to check integrity TBD **)
  if e then (
    printf "get_index_with_init: index file exists\n%!";
    Index.read_mbox_header file >>= function
      | `Ok header -> return (Ok header)
      | `Eof i -> return (Error "End of file")
  )
  else (
    printf "get_index_with_init: index file doesn't exist\n%!";
    Index.create_index_file file >>= fun res ->
    if res = false then
      return (Error "Failed to create index file")
    else (
      printf "initialized mbox\n%!";
      (** index doesn't exist, try to get the data from the mailbox **)
      if with_mbox then
        rebuild_index mbx name >>= function
        | Error e -> return (Error e)
        | Ok header ->
          if header.uidvalidity = "" then
            let header = {header with uidvalidity=(Index.new_uidvalidity())} in
            Index.write_mbox_header file header >>= fun () -> return (Ok header)
          else
            return (Ok header)
      else
        let header = (Index.empty_mbox_header ~uidvalidity:(Index.new_uidvalidity())()) in
        Index.write_mbox_header file header >>= fun () -> return (Ok header)
    )
  )


(** select a mailbox 
 * assuming that if index file exists it must be up-to-date
 * need to redo Invalid/Directory/etc TBD
**)
let select_ mbx name (read_only:bool) : ([`NotExists|`NotSelectable|`Error of string|`Ok of t * Index.mbox_header] Deferred.t) =
  printf "select_\n%!";
  valid_mailbox mbx name >>= function
  | `NotExists -> return `NotExists
  | `NotSelectable -> return `NotSelectable
  | `ValidMailbox -> 
    get_index_with_init ~with_mbox:true mbx name >>= function
    | Error e -> return (`Error e)
    | Ok header -> 
        let sel =
        (
        if read_only = true then
          Some (Select name)
        else
          Some (Examine name)
        ) in
        return (`Ok((update mbx sel),header))

(** select mailbox **)
let select mbx name : 
  ([`NotExists|`NotSelectable|`Error of string|`Ok of t * Index.mbox_header] Deferred.t) =
  select_ mbx name false

(** examine mailbox, same as select but read-only **)
let examine mbx name : 
  ([`NotExists|`NotSelectable|`Error of string|`Ok of t * Index.mbox_header] Deferred.t) =
  select_ mbx name true


(** close mailbox **)
let close mbx =
  (update mbx None)
  
(** email root folder -> relative root -> maillbox [wildcards] -> recursive
 * relative path from the relative root -> list of mailboxes/flags 
 * recursively list directory content **)
let rec listmbx_ mailbxs (reference:string) (mailbox:string) (prefix:string)
(filter:('a,'b,'comp) Map.t) : (string *string list) list Deferred.t =
  (** fix regex for the mailbox **)
  let regxmailbox = fixregx_mbox mailbox in
  (** get mailboxes root **)
  let (_, mbxs_root, _, _) = mailbxs in 
  (** full path search directory **)
  let dir = make_path [mbxs_root; reference; prefix] in

  is_directory dir >>= fun is_dir ->

  if is_dir = false then
    return ([])
  else
  (
    Sys.ls_dir dir >>= 
    (fun l ->
      Deferred.List.fold l 
      ~init:[]
      ~f:
      (fun acc item -> 
        if match_dot item then 
          return (acc)
        else
        (
          (** get item full path **)
          let path = concat_path dir item in
          (** get item path relative to the relative root **)
          let filtered = (Map.is_empty filter) = false &&
            (match Map.find filter (concat_path prefix item) with None -> true| Some _ -> false) in
          let tomatch = make_path [reference; prefix; item] in
          is_directory path >>= 
          (** recurse into directory to get more items **)
          (fun isdir ->
            let item = concat_path prefix item in
            if isdir then
            (
              listmbx_ mailbxs reference mailbox item filter >>= 
              (fun l -> 
                if match_regex tomatch regxmailbox && filtered = false then
                    return (List.concat [[(dir_item item l)];acc;l])
                else
                  return (List.concat [acc; l])
              )
            ) else if match_regex tomatch regxmailbox && filtered = false then
              return ((mbox_item item reference)::acc)
            else
              return (acc)
          )
        )
      )
    )
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
    listmbx_ mailbx fixref fixmbx "" filter >>= 
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
  let path = get_subscr_path mbx in
  file_fold path ~acc:(Map.empty ~comparator:String.comparator) ~cb:(fun acc line ->
    Map.add acc ~key:line ~data:0) >>= fun acc ->
    listmbx_filt mbx reference mailbox acc

(** create mailbox **)
let create_mailbox mbx (mailbox:string) : [`Error of string|`Ok] Deferred.t =
  let path = get_mbox_path mbx mailbox in
  file_exists path >>= fun e ->
  if e then 
    return (`Error("Mailbox already exists"))
  else (
    is_directory (Filename.dirname path) >>= fun res ->
    if res = false then
      return (`Error("Invalid Superior"))
    else
    (
      if match_regex mailbox "^/" then
        return (`Error("Invalid mailbox name: Begins with hierarchy separator"))
      else if match_regex mailbox "^\"?.imaplet/?\"?" then
        return (`Error("Invalid mailbox name: Contains reserved name"))
      else if match_regex mailbox "^\"?./?\"?$" || match_regex mailbox "^\"?../?\"?$" then
        return (`Error("Invalid mailbox name: Contains . part"))
      else if match_regex mailbox "[^/]+/[\"]?$" then (** a folder **)
      (
        mkdir ~perm:0o777 ~parent:true path >>= fun res ->
        if res then
          return (`Ok)
        else
          return (`Error("Failed to create the Mailbox"))
      ) else ( (** mailbox **)
        get_index_with_init mbx mailbox >>= function 
        | Error e -> return (`Error e)
        | Ok (recs) ->
        init_mailbox mbx mailbox recs >>= fun res ->
          return (`Ok)
      )
    )
  )

(** delete mailbox **)
let delete_mailbox mbx mailbox : [`Error of string|`Ok] Deferred.t =
  let path = get_mbox_path mbx mailbox in
  file_exists path >>= fun e ->
  if e = false then 
    return (`Error("Mailbox doesn't exist"))
  else (
    is_directory (Filename.dirname path) >>= fun res ->
    if res = false then (** regular mailbox - delete the index and the mailbox **)
    (
      let path = get_index_path mbx mailbox in
      delete_file path >>= fun res ->
      if res = false then
        return (`Error("Failed to delete the mailbox"))
      else (
        let box = get_mbox_path mbx mailbox in
        delete_file box >>= fun res ->
        if res = false then
          return (`Error("Failed to delete the mailbox"))
        else
          return (`Ok)
      )
    ) else ( (** directory **)
      try_with (fun() -> Unix_syscalls.system_exn ("rm -rf " ^ (Regex.squote path))) >>= function
      | Ok _ -> return (`Ok)
      | Error _ -> return (`Error("Failed to delete the mailbox"))
    )
  )

(** rename a mailbox **)
let rename_mailbox mbx (src:string) (dst:string) : 
  [`Error of string|`Ok] Deferred.t =
  if match_regex ~case:false src "inbox" then
    (** need to copy the content to the new mailbox
     * and need to change uidvalidity, should do once
     * can update mailbox TBD
     **)
    return (`Error("Can't rename INBOX"))
  else (
    let src_path = get_mbox_path mbx src in
    let dst_path = get_mbox_path mbx dst in
    file_exists src_path >>= fun e ->
    if e = false then 
      return (`Error("Mailbox doesn't exist: " ^ src))
    else (
      file_exists dst_path >>= fun e ->
      if e  then
        return (`Error("Mailbox already exists: " ^ dst))
      else (
        (** create destination folder if doesn't exist **)
        mkdir ~perm:0o777 ~parent:true (Filename.dirname dst_path) >>= fun res ->
        if res = false then
          return (`Error("Failed to rename the mailbox"))
        else (
          is_directory src_path >>= fun e ->
          if e then (** renaming directory **)
          (
            rename src_path dst_path >>= fun res ->
            if res then
              return (`Ok)
            else
              return (`Error("Failed to rename the mailbox"))
          ) else ( (** renaming a file **)
            Index.init_mbox_header (get_index_path mbx dst) >>= fun res ->
            if res = false then
              return (`Error("Failed to rename the mailbox"))
            else (
              let src_index = get_index_path mbx src in
              let dst_index = get_index_path mbx dst in
              rename src_index dst_index >>= fun res ->
              if res = false then
                return (`Error("Failed to rename the mailbox"))
              else (
                rename src_path dst_path >>= fun res ->
                if res then
                  return (`Ok)
                else
                  return (`Error("Failed to rename the mailbox"))
              )
            )
          )
        )
      )
    )
  )

(** subscribe a mailbox **)
let subscribe mbx (mailbox:string) : [`Ok|`Error of string] Deferred.t =
  let path = get_mbox_path mbx mailbox in
  file_exists path >>= fun res ->
  if res = false then
    return (`Error("Mailbox doesn't exist"))
  else
  (
    (** the .subscription must be created when account is created TBD **)
    let path = get_subscr_path mbx in
    file_fold path ~acc:false ~cb:(fun acc line ->
      if line = mailbox then (** already subscribed **)
        true
      else
        acc
    ) >>= fun res ->
      if res = false then
      (
        try_with ( fun () ->
          Writer.with_file path ~append:true ~exclusive:true ~f:(fun w ->
            Writer.write_line w mailbox; return())
        ) >>= function
          | Ok() -> return (`Ok)
          | Error _ -> return (`Error("Failed to subscribe"))
      )
      else
        return (`Ok)
  )

(** unsubscribe a mailbox **)
let unsubscribe mbx (mailbox:string) : [`Error of string|`Ok] Deferred.t =
  let path = get_subscr_path mbx in
  let lock = path ^ ".lock" in
  try_with ( fun () ->
    Writer.with_file lock ~exclusive:true ~f:(fun _ ->
      file_fold path ~acc:([],false) ~cb:(fun acc line ->
        let (l,b) = acc in
        if line = mailbox then
          (l,true)
        else
          (List.concat [l;[line]], b)
      ) >>= fun (l,b) ->
        if b then (
          Writer.with_file path ~exclusive:true ~f:(fun w ->
           List.iter l ~f:(fun item -> Writer.write_line w item);
           return `Ok
          ) 
        ) else
          return  `Ok
    )
  ) >>= function 
      | Ok res -> return res
      | Error _ -> return (`Error("Failed to unsubscribe"))

(** get status, should have it's own status, don't call examine TBD there might
 * performance issue depending on future implementation **)
let get_status mbx mailbox optlist :
  ([`NotExists|`NotSelectable|`Error of string|`Ok of mbox_header] Deferred.t) =
  examine mbx mailbox >>= function
    | `NotExists -> return `NotExists
    | `NotSelectable -> return `NotSelectable
    | `Error e -> return (`Error(e))
    | `Ok(selected,stat) -> return(`Ok(stat))

(**
 * append the message to the mailbox
 * accumulate statistics for the index header
 * write index record for the message
 **)
let do_append (write_record_to_index:(mbox_msg_metadata -> unit Deferred.t))
(writer:Writer.t) (header:mbox_header) (message:Mailbox.Message.t) 
(flags:States.flags list option) (date:Time.t option) : (mbox_header Deferred.t) =
  let record = empty_mbox_msg_metadata() in
  let uid = header.uidnext in
  let internal_date = match date with | None -> Time.now() | Some date -> date in
  let flags = match flags with | None -> ["\\Recent"] | Some flags -> List.dedup
  ~compare:String.compare (List.fold flags
  ~init:["\\Recent"] ~f:(fun flags flag -> (States.fl_to_str flag)::flags)) in
  let headers = Header.to_list (Email.header message.email) in
  let init_headers =
  (if header.count = 0 then 
    let imapbase = sprintf "%s %010d NotJunk $NotJunk" header.uidvalidity header.uidnext in (** what other flags? **)
    [("X-IMAPbase",imapbase);("Status","RO");("X-UID",string_of_int uid)]
  else
    [("Status","RO");("X-UID",string_of_int uid)]
  ) in
  let headers = List.fold headers ~init:init_headers ~f:(fun headers (name,value) ->
    if name <> "Status" && name <> "X-UID" && name <> "X-IMAPbase" && name <> "X-IMAP" then
      (name,value)::headers
    else
      headers
  ) in
  Index.cur_offset (Writer.fd writer) >>= fun offset -> 
  let start_offset = offset in
  let header = {header with count = header.count + 1; uidnext = uid+1} in
  let email = Email.set_header message.email (Header.of_rev_list headers) in
  Writer.write writer (Mailbox.Postmark.to_string message.postmark);
  Writer.write_line writer "";
  Writer.write writer (Email.to_string email);
  Index.cur_offset (Writer.fd writer) >>= fun offset ->
  let size = (match (Email.raw_content email) with
    | None -> 0
    | Some cont -> Octet_stream.length cont
  ) in
  let end_offset = offset in
  let record = {record with uid;internal_date;size;flags;start_offset;end_offset} in 
  write_record_to_index record >>= fun () -> 
  return header

(** iterate over new mailbox structure, possibly with multiple messages,
 * rebuild index file if doesn't exist (expensive TBD)
 * accumulate data for the index header update
 **)
let open_mbox_append mbx (name:string) (flags:States.flags list option)
(date:Time.t option) (mailbox:Mailbox.With_pipe.t) 
cb : (mbox_header,string) Result.t Deferred.t =
  get_index_with_init ~with_mbox:true mbx name >>= function
  | Error e -> return (Error e)
  | Ok index_header -> 
  let index_file = get_index_path mbx name in
  Index.write_with_file_on_cb index_file true (** write record to index **)
  (fun write_record_to_index ->
    let file = get_mbox_path mbx name in
    Writer.with_file ~append:true ~exclusive:true file 
    ~f:(fun writer ->
      Pipe.fold mailbox
      ~init:(index_header)
      ~f:(fun index_header message -> cb write_record_to_index writer index_header message flags date)
    ) >>= fun header -> return (Ok header)
  ) >>= function
    | Error e -> return (Error e)
    | Ok header -> return (Ok header)

(** append to the mailbox, read from the network more data per literal
 * write to the client before reading unles + literal 
 * the data is parsed into new mailbox structure for further processing
 * if the mailbox consists of only the template header message then this message
 * should be overwritten! TBD
 **)
let append mbx (name:string) (reader:Reader.t) (writer:Writer.t) (flags:States.flags list option)
  (date:Time.t option) (literal:States.literal) :
    ([`NotExists|`NotSelectable|`Eof of int|`Error of string|`Ok of mbox_header] Deferred.t) =
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
      try_with (fun() ->
        (** if short message then read it into memory **)
        if size < Configuration.max_message_in_memory_size then (
          let buff = String.create size in
          Reader.really_read reader ~pos:0 ~len:size buff >>= function
          | `Eof i -> return (`Eof i)
          | `Ok ->
              (** passing buffer dirrectly doesn't work most of the time
               * it looks like the parser fails to match the postmark
               * message:
                 * : Maybe missing >.stmark From dovecot@localhost.local  Thu
                 * Dec 26 17:16:43 2013
                 * No message context for: Date: Mon, 7 Feb 1994 21:52:25 -0800
                 * (PST)
               * but saving to a file and then reading from the file works. TBD **)
              Mailbox.With_pipe.of_string buff >>= fun mailbox ->
              open_mbox_append mbx name flags date mailbox do_append >>= function
              | Error e -> return (`Error e)
              | Ok header -> return (`Ok header)
        ) else ( (** write into a temp file **)
          let tmp = "tmp." ^ Index.new_uidvalidity() in
          Writer.with_file tmp ~exclusive:true
          ~f:(fun w -> read_literal reader size (fun res buff -> Writer.write w buff) ) >>= function 
            | `Eof i -> delete_file tmp >>= fun _ -> return (`Eof i)
            | `Ok -> 
              Mailbox.With_pipe.t_of_file tmp >>= fun mailbox ->
              open_mbox_append mbx name flags date mailbox do_append >>= function
              | Error e -> return (`Error e)
              | Ok header -> delete_file tmp >>= fun _ -> return (`Ok header)
        )
      ) >>= function
        | Error e -> return (`Error (Exn.to_string e))
        | Ok res -> return res

(** reads the mailbox/index passing to the cb the message and corresponding * index **)
let read_mailbox_with_file (mbx:t) ~(init:'a) ~(cb:(int -> 'a -> Mailbox.Message.t ->
  Index.mbox_msg_metadata -> 'a Deferred.t)) : 
    ([`NotExists|`NotSelectable|`Error of string|`Ok of 'a] Deferred.t) =
  match (selected_mbox mbx) with
  | None -> return (`Error ("Not Selected"))
  | Some name ->
    Index.read_index_with_file (get_index_path mbx name) ~skip:Header (fun the_reader ->
      (** for efficiency: pre-search the searh keys to see if index access is required TBD **)
      mbox_fold mbx name ~acc:(init,0,None) 
      ~cb:(fun mailbox (acc,seq,error) message ->
        let seq = seq + 1 in
        the_reader () >>= function
          | `Eof i -> Pipe.close_read mailbox; return (acc,seq,Some ("Index failure")) (** index integrity failure TBD **)
          | `Header _ -> Pipe.close_read mailbox; return (acc,seq, Some ("Index failure (pos)"))
          | `Record record -> 
              cb seq acc message record >>= fun acc -> return (acc,seq,None)
      )
    ) >>= function
      | `NotExists -> return `NotExists
      | `NotSelectable -> return `NotSelectable
      | `Ok (acc,seq,error) ->
          match error with
          | Some error -> return (`Error error)
          | None -> return (`Ok acc)

(** find messages matching the search criteria
**)
let search mbx (keys:('a)States.searchKeys) (buid:bool) : 
  [`NotExists|`NotSelectable|`Error of string|`Ok of int list] Deferred.t =
    read_mailbox_with_file mbx 
    ~init:[]
    ~cb:(fun seq acc message record ->
        let res = Interpreter.exec_search message.email keys record seq in 
        if res then
          let selected = if buid then record.uid else seq in
          return (selected :: acc)
        else
          return (acc)
    )

let fetch (mbx:t) (resp_writer:(string->unit)) (sequence:States.sequence) (fetchattr:States.fetch)
(buid:bool) : [`NotExists|`NotSelectable|`Error of string|`Ok of unit] Deferred.t = 
    read_mailbox_with_file mbx 
    ~init:()
  ~cb:(fun seq () message record ->
    let res = Interpreter.exec_fetch seq sequence message record fetchattr buid in
    match res with
    | Some res -> resp_writer res; return ()
    | None -> return ()
  )

let update_header seq header record =
  let unseen,nunseen,recent =
  if seq = 1 then
    0,0,0
  else (
    let seen = List.find record.flags ~f:(fun fl -> fl = "\\Seen") in
    let recent = List.find record.flags ~f:(fun fl -> fl = "\\Recent") in
    let unseen =
      if seen = None && header.unseen <> 0 then
        seq
      else
        header.unseen
    in
    let nunseen =
      if seen = None then
        header.nunseen + 1
      else
        header.nunseen 
    in
    let recent =
      if recent <> None then
        header.recent + 1
      else
        header.recent
    in
    unseen,nunseen,recent
  )
  in
  update_mbox_header ~header ~unseen ~nunseen ~recent () 

let store (mbx:t) (resp_writer:(string->unit)) (sequence:States.sequence)
(storeattr:States.storeFlags) (flagsval:States.flags list)
(buid:bool) : [`NotExists|`NotSelectable|`Error of string|`Ok of unit] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
    valid_mailbox mbx name >>= function
    | `NotExists -> return `NotExists
    | `NotSelectable -> return `NotSelectable
    | `ValidMailbox ->
    let file = get_index_path mbx name in
    (read_write_with_file_on_cb file (fun seq header record ->
      let record =
      match Interpreter.exec_store record seq sequence storeattr flagsval buid with
      | `None -> record
      | `Silent record -> record
      | `Ok (record,res) -> resp_writer res;record
      in
      (update_header seq header record,record)
    )) >>= function
    | Ok() -> return (`Ok())
    | Error e -> return (`Error e)

(** iterate over new mailbox structure, possibly with multiple messages,
 * rebuild index file if doesn't exist (expensive TBD)
 * accumulate data for the index header update 
 * this is almost identical to open_mbox_append TBD
 * if tmp is true then the dest_mbox after complition is moved
 * to mbx, and index file for dest_mbox is moved to the index of mbx
 * need to handle error conditions, need to make the index file atomic too
 * TBD!!!!
 **)
let open_mbox_copy mbx (dest_mbox:string) (tmp:bool) 
 (filter:(int->Index.mbox_header->Mailbox.Message.t->Index.mbox_msg_metadata->bool)) :
  (mbox_header,string) Result.t Deferred.t =
  get_index_with_init ~with_mbox:true mbx dest_mbox >>= function
  | Error e -> return (Error e)
  | Ok index_header -> 
  let index_file = get_index_path mbx dest_mbox in
  Index.write_with_file_on_cb index_file true (** append record to index **)
  (fun write_record_to_index ->
    let file,with_file = 
    if tmp = false then (
      let file = get_mbox_path mbx dest_mbox in
      file, Writer.with_file ~append:true ~exclusive:true
    ) else (
      let file = Option.value_exn (selected_mbox mbx) in
      let file = get_mbox_path mbx file in
      let tmp_file = get_mbox_path mbx dest_mbox in
      file,Writer.with_file_atomic ~temp_file:tmp_file ~fsync:true
    )
    in
    with_file file
    ~f:(fun writer ->
      read_mailbox_with_file mbx
      ~init:(index_header)
      ~cb:(fun seq header message record -> 
        if (filter seq header message record) then
          do_append write_record_to_index writer header message 
          (Some (States.lstr_to_fl record.flags)) None 
        else
          return header
      )
    ) >>= function
      | `Ok header -> return (Ok header)
      | `Error e -> return (Error e)
      | `NotExists -> return (Error "Doesn't exist")
      | `NotSelectable -> return (Error "Not selectable")
  ) >>= function
    | Error e -> return (Error e)
    | Ok header -> 
        if tmp then
          let file = Option.value_exn (selected_mbox mbx) in
          let file = get_index_path mbx file in
          rename index_file file >>= fun _ ->
          delete_file (get_mbox_path mbx dest_mbox) >>= fun _ ->
          return (Ok header)
        else
          return (Ok header)

(** if copy fails it should restore the mailbox, so need to copy TBD **)
let copy (mbx:t) (dest_mbox:string) (sequence:States.sequence) (buid:bool) :
[`NotExists|`NotSelectable|`Error of string|`Ok of unit] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
    valid_mailbox mbx dest_mbox >>= function
    | `NotSelectable -> return `NotSelectable
    | `NotExists -> return `NotExists
    | `ValidMailbox -> 
      open_mbox_copy mbx dest_mbox false
      (fun seq header message record -> 
        let id = (if buid then record.uid else seq) in
        (Interpreter.exec_seq sequence id)
      ) >>= function
      | Error e -> return (`Error e)
      | Ok header -> return (`Ok ())

let create_tmp tmp =
  printf "create_tmp %s\n%!" tmp;
  Unix_syscalls.system_exn ("touch " ^ (Regex.squote tmp)) >>= fun () ->
    Unix_syscalls.system_exn ("chmod 666 " ^ (Regex.squote tmp))
  (**
  Unix_syscalls.openfile tmp ~mode:[`Creat;`Wronly;`Trunc] >>= fun fd ->
    let w = Writer.create fd in
    Writer.write_byte w 0; Writer.flushed w >>= fun() ->
      Writer.close w >>= fun() ->
        Unix_syscalls.close ~should_close_file_descriptor:true fd
        **)

let create_tmp_all mbx name =
  let tmp = name ^ "." ^ Index.new_uidvalidity() in
  let tmp_mbox = get_mbox_path mbx tmp in
  let tmp_index = get_index_path mbx tmp in
  create_tmp tmp_mbox >>= fun () ->
  (**create_tmp tmp_index >>= fun () ->**)
    return tmp

(** permanently remove messages with \Deleted flag set 
 * create a temp mailbox, copy filtered content to the box
 * need to revert if fails TBD
**)
let expunge (mbx:t) (resp_writer:(string->unit)) : 
  [`Error of string|`Ok] Deferred.t = 
  match (selected_mbox mbx) with
  | None -> return (`Error "Not selected")
  | Some name ->
    create_tmp_all mbx name >>= fun tmp_mbox ->
    open_mbox_copy mbx tmp_mbox true
    (fun seq header message record -> 
      if (List.find record.flags ~f:(fun fl -> fl = "\\Deleted")) = None then
        true
      else (
        resp_writer ((string_of_int seq) ^ " EXPUNGE");
        false
      )
    ) >>= function
    | Error e -> return (`Error e)
    | Ok header -> return `Ok
