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
open Lwt
open Email_message
open StorageMeta
open Sexplib

let path = IrminStorageConfig.store_path
module Git =
  IrminGit.Make(IrminKey.SHA1)(IrminContents.String)(IrminReference.String)
  module Store = (val Git.create ~bare:true ~kind:`Disk ~root:path ())

module IrminsuleIntf =
  struct

    let key_to_string key = 
     let open Core.Std in
     List.fold key 
     ~init:""
     ~f:(fun acc item -> 
       if acc = "" then
         "/" ^ item
       else
         acc ^ "/" ^ item
     ) 

  (* convert irmin key to Unix path *)
    let key_to_path key = 
     let open Core.Std in
     List.foldi key 
     ~f:(fun i acc item -> 
       if i < 2 then (* skip imaplet and user *)
         acc
       else if i % 2 = 0 then ( (* skip mailboxes *) 
         if acc = "" then
           acc
         else
           acc ^ "/" 
       ) else 
         acc ^ item
       ) ~init:""

    let create () =
      Store.create ()

    let update store key data =
      (*Printf.printf "------ store update %s\n" (key_to_string key);*)
      Store.update store key data

    let update_view store key view =
      (*Printf.printf "------ store update_view %s\n" (key_to_string key);*)
      Store.update_view store key view

    let read store key =
      Store.read store key

    let read_exn store key =
      Store.read_exn store key

    let read_view store key =
      Store.read_view store key

    let list store key =
      (*Printf.printf "------ store list %s\n" (key_to_string (List.hd key));*)
      Store.list store key

    let remove store key =
      (*Printf.printf "------ store remove %s\n" (key_to_string key);*)
      Store.remove store key

    let mem store key =
      Store.mem store key

  end

exception NotFound

(* store the message broken down into postmark, header, content
 * stored under mailbox/messages key
 * mailbox/messages/postmark
 * mailbox/messages/headers
 * mailbox/messages/content
 * mailbox/messages/meta - per message metadata like flags, uid, etc
 *)
module MailboxMessage =
  struct
    (* uid * list of key parts *)
    type t = (string * string list)

    let mk_key t key_type = 
      let append t key =
        let (uid,mailbox) = t in
        match key with
        | Some key -> List.concat [mailbox;[uid];[key]]
        | None -> List.concat [mailbox;[uid]]
      in
      match key_type with
      | `Headers -> append t (Some "headers")
      | `Email -> append t (Some "content")
      | `Meta -> append t (Some "meta")
      | `Postmark -> append t (Some "postmark")
      | `Root -> append t None

    (* mailbox has all key parts including the name of the mailbox *)
    let create mailbox uid = 
      (uid, List.concat [mailbox;["messages"]])

    (* adde messge and message metadata *)
    let add t (message:Mailbox.Message.t) message_metadata =
      Printf.printf "adding messge with uid %d\n%!" message_metadata.uid;
      IrminsuleIntf.create () >>= fun s ->
      (*let message = Mailbox.Message.t_of_sexp (Sexp.of_string message) in*)
      let postmark = message.postmark in
      let postmark_sexp = Mailbox.Postmark.sexp_of_t postmark in
      let email = message.email in
      let email_sexp = Email.sexp_of_t email in
      let header = Email.header email in
      let header_sexp = Header.sexp_of_t header in
      let update t s key_type str =
        let key = mk_key t key_type in
        IrminsuleIntf.update s key str
      in
      update t s `Postmark (Sexp.to_string postmark_sexp) >>= fun () ->
      update t s `Headers (Sexp.to_string header_sexp) >>= fun () ->
      (* should break email into separate header and content sexp TBD *)
      update t s `Email (Sexp.to_string email_sexp) >>= fun () ->
      let headerm = Header.to_string_monoid header in
      let sexp = sexp_of_mailbox_message_metadata message_metadata in
      update t s `Meta (Sexp.to_string sexp)

    (* remove the message *)
    let remove t =
      IrminsuleIntf.create () >>= fun s ->
      let key = mk_key t `Root in
      IrminsuleIntf.remove s key

    (* read message *)
    let read_message t =
      try
        IrminsuleIntf.create () >>= fun s ->
        let read t s key_type =
          let key = mk_key t key_type in
          IrminsuleIntf.read s key >>= function
          | Some value -> return value
          | None -> Printf.printf "key not found%s\n%!" (IrminsuleIntf.key_to_string key ); raise NotFound
        in
        read t s `Postmark >>= fun postmark_sexp_str ->
        read t s `Email >>= fun email_str ->
        let postmark_sexp = Sexp.of_string postmark_sexp_str in
        let postmark = Mailbox.Postmark.t_of_sexp postmark_sexp in
        let email_sexp = Sexp.of_string email_str in
        let email = Email.t_of_sexp email_sexp in
        let message =
          {Mailbox.Message.postmark=postmark;Mailbox.Message.email=email} in
        return (`Ok message)
      with _ ->
        return `NotFound

    (* read message metadata *)
    let read_meta t =
      IrminsuleIntf.create () >>= fun s ->
      let key = mk_key t `Meta in
      IrminsuleIntf.read s key >>= function
      | Some str ->
        let sexp = Sexp.of_string str in
        return (`Ok (mailbox_message_metadata_of_sexp sexp))
      | None -> return `NotFound

    let read_message_and_meta t =
      read_message t >>= function
      | `NotFound -> return `NotFound
      | `Ok message -> read_meta t >>= function
        | `NotFound -> assert(false)
        | `Ok meta -> return (`Ok (message, meta))

    (* update message metadata *)
    let update_metadata t metadata =
      IrminsuleIntf.create () >>= fun s ->
      let key = mk_key t `Meta in
      IrminsuleIntf.mem s key >>= fun res ->
      if res = false then
        return `NotFound
      else
        let sexp = sexp_of_mailbox_message_metadata metadata in
        IrminsuleIntf.update s key (Sexp.to_string sexp) >>= fun () ->
        return `Ok

    (* list uid of messages *)
    let list_messages t =
      let (uid,mailbox) = t in
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.list s [mailbox] >>= fun l ->
      return (Core.Std.List.fold l ~init:[] ~f:(fun acc i ->
        (IrminsuleIntf.key_to_string i) :: acc
      ))

  end

(* index for message sequence number
 * the key mailbox/index/sequence/uids
 * for now just a simple list, need to change TBD *)
module MailboxIndex =
  struct
    type t = string list

    (* mailbox has all key parts including the name of the mailbox *)
    let create mailbox =
      List.concat [mailbox;["index"];["sequence"];["uids"]]

    (* update the complete index list *)
    let update_uids s key uids =
      let sexp = Core.Std.List.sexp_of_t (fun i -> Sexp.of_string i) uids in
      IrminsuleIntf.update s key (Sexp.to_string sexp) 

    (* get complete index list *)
    let get_uids s key =
      IrminsuleIntf.read s key >>= function
      | Some str ->
        let sexp = Sexp.of_string str in
        return (Core.Std.List.t_of_sexp (fun i -> Sexp.to_string i) sexp)
      | None -> update_uids s key [] >>= fun () -> return []

    (* add uid to the index list *)
    let add key uid =
      IrminsuleIntf.create () >>= fun s ->
      get_uids s key >>= fun uids ->
      update_uids s key (Core.Std.List.concat [uids;[uid]])

    (* remove uid from the index list *)
    let remove key uid = 
      IrminsuleIntf.create () >>= fun s ->
      get_uids s key >>= fun uids ->
      update_uids s key (Core.Std.List.fold_right uids ~init:[] 
      ~f:(fun i acc -> 
        if i = uid then 
          acc
        else
          i :: acc
      ))

    (* find uid corresponding to the sequence *)
    let get_uid key seq =
      assert(seq > 0);
      let seq = seq - 1 in
      IrminsuleIntf.create () >>= fun s ->
      get_uids s key >>= fun uids ->
      match (Core.Std.List.nth uids seq) with
      | Some uid -> return (`Ok uid)
      | None -> Printf.printf "no uids found %s %d\n%!" (IrminsuleIntf.key_to_string key) seq; return `NotFound

    (* find sequence corresponding to uid *)
    let get_seq key uid =
      IrminsuleIntf.create () >>= fun t ->
      get_uids t key >>= fun uids ->
      let res = Core.Std.List.findi uids ~f:(fun i u -> if u = uid then true else false)
      in match res with
      | Some (i,_) -> return (`Ok i)
      | None -> return `NotFound

  end

(* pet mailbox metadata
 * mailbox/meta key *)
module MailboxMetadata =
  struct
    type t = string list

    (* create type *)
    let create key =
      List.concat [key;["meta"]]

    (* update the metadata *)
    let update key meta =
      Printf.printf "------------- updating metadata %s\n%!" meta.uidvalidity;
      IrminsuleIntf.create () >>= fun s ->
      let sexp = sexp_of_mailbox_metadata meta in
      IrminsuleIntf.update s key (Sexp.to_string sexp)

    (* get the metadata *)
    let read key =
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.read s key >>= function
      | Some str ->
        let sexp = Sexp.of_string str in
        return (mailbox_metadata_of_sexp sexp)
      | None ->
        let meta = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity()) () in
        update key meta >>= fun () ->
        return meta

    (* does the meta key exists?
     * it indicates if the mailbox exists or not
     *)
    let exists key =
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.mem s key

  end

(* mailboxes subscription *)
module MailboxesSubscription =
  struct
    type t = string list

    (* create type *)
    let create user =
      Core.Std.List.concat [user;["subscription"]]

    (* convert the list to a string of sexp *)
    let str_sexp_of_list l =
      let sexp = Core.Std.List.sexp_of_t (fun i -> Sexp.of_string i) l in
      Sexp.to_string sexp

    (* convert string of sexp to the list *)
    let list_of_str_sexp str =
      let sexp = Sexp.of_string str in
      (Core.Std.List.t_of_sexp (fun i -> Sexp.to_string i) sexp)

    (* update subscription list *)
    let update s key l =
      let str = str_sexp_of_list l in
      IrminsuleIntf.update s key str

    (* read subscription *)
    let read s key =
      IrminsuleIntf.read s key >>= function
      | Some str -> return (list_of_str_sexp str)
      | None -> return []

    (* subscribe *)
    let subscribe key mbox =
      IrminsuleIntf.create () >>= fun s ->
      read s key >>= fun l ->
      if (Core.Std.List.find l ~f:(fun i -> if i = mbox then true else false) <> None) then 
        return ()
      else
        update s key [mbox]

    (* unsubscribe *)
    let unsubscribe key mbox =
      IrminsuleIntf.create () >>= fun s ->
      read s key >>= fun l ->
      let l = Core.Std.List.fold_right l ~f:(fun i acc -> if i = mbox then acc else i::acc) ~init:[] in
      update s key [mbox]

    (* get subscription *)
    let get_subscription key =
      IrminsuleIntf.create () >>= fun s ->
      read s key 
  end

module type IrminMailbox_intf =
  sig
    type t

    (* irmin key [..] *)
    val create : string -> string -> t

    (* check if the mailbox exits *)
    val exists : t -> [`No|`Folder|`Storage] Lwt.t

    (* add message to the store *)
    val add : t -> Mailbox.Message.t -> mailbox_message_metadata -> unit Lwt.t

    (* remove message from the store *)
    val remove : t -> string -> unit Lwt.t

    (* read message/metadata *)
    val read_message : t -> ?filter:(States.searchKey) States.searchKeys -> int -> [`Ok of (Mailbox.Message.t *
    mailbox_message_metadata)| `NotFound|`Eof] Lwt.t

    (* read metadata only *)
    val read_metadata : t -> int -> [`Ok of mailbox_message_metadata| `NotFound] Lwt.t

    (* update metadata *)
    val update_metadata : t -> int -> mailbox_message_metadata -> [`Ok|`NotFound] Lwt.t

    (* create the mailbox *)
    val create_mailbox : t -> ?folders:bool -> unit Lwt.t

    (* delete the mailbox *)
    val delete_mailbox : t -> unit Lwt.t

    (* move the mailbox *)
    val move_mailbox : t -> t -> unit Lwt.t

    (* copy the mailbox *)
    val copy_mailbox : t -> t -> filter:(bool*States.sequence) -> [`Ok|`SrcNotExists|`DestNotExists] Lwt.t

    (* expunge deleted messages *)
    val expunge : t -> unit Lwt.t

    (* list storage *)
    val list_store : t -> [`Folder of string*int|`Storage of string] list Lwt.t

    (* search messages *)
    val search_with : t -> (bool*(States.searchKey) States.searchKeys) -> int list Lwt.t

    (* get mailbox metadata *)
    val get_mailbox_metadata : t -> mailbox_metadata Lwt.t

    (* get subscription *)
    val get_subscription :t -> string list Lwt.t

    (* subscribe *)
    val subscribe : t -> string -> unit Lwt.t

    (* unsubscribe *)
    val unsubscribe : t -> string -> unit Lwt.t

    (* create user account *)
    val create_account : t -> [`Ok|`Exists] Lwt.t

    (* remove account *)
    val remove_account : t -> [`Ok|`DoesntExist] Lwt.t

    val to_string : t -> string

    val show_all : t -> unit Lwt.t

  end

(* Irminsule Mailbox - one acces to all other modules *)
module IrminMailbox =
  struct
    type t = string list

    (* create type *)
    let create user path = 
      let l = Str.split (Str.regexp "/") path in
      let l = Core.Std.List.fold_right l ~f:(fun i acc -> "mailboxes" :: i :: acc) ~init:[] in
      let l = "imaplet" :: user :: l in
      l

    (* find the flag *)
    let find_flag l fl = 
      Core.Std.List.find l ~f:(fun f -> if f = fl then true else false) <> None

    (* update per mailbox metadata with the message metadata *)
    let update_mailbox_meta key message_meta ~adding ~countup ~uidnextup ~up =
      let meta_key = MailboxMetadata.create key in
      MailboxMetadata.read meta_key >>= fun mailbox_meta ->
      let count = mailbox_meta.count + countup in
      let uid = mailbox_meta.uidnext in
      let uidnext = mailbox_meta.uidnext + uidnextup in
      let seen = find_flag message_meta.flags Flags_Seen in
      let nunseen = 
        if seen = false then 
          mailbox_meta.nunseen + up 
        else
          mailbox_meta.nunseen 
      in
      let recent =
        if find_flag message_meta.flags Flags_Recent = true then 
          mailbox_meta.recent + up 
        else 
          mailbox_meta.recent
      in
      let unseen =
        if adding then (
          if mailbox_meta.unseen = 0 && seen = false then
            count
          else
            mailbox_meta.unseen
        ) else
          mailbox_meta.unseen
      in
      let mailbox_meta = update_mailbox_metadata 
        ~header:mailbox_meta ~uidnext ~nunseen ~unseen ~recent ~count () in
      MailboxMetadata.update meta_key mailbox_meta >>= fun () -> return uid

    (* add message and message metadata to the store *)
    let add key message metadata =
      update_mailbox_meta key metadata ~adding:true ~countup:1 ~uidnextup:1 ~up:1 >>= fun uidnext ->
      let uid = string_of_int uidnext in
      let msg_key = MailboxMessage.create key uid in
      (*let message = Sexp.to_string (Mailbox.Message.sexp_of_t message) in *)
      let size = String.length (Mailbox.Message.to_string message) in
      MailboxMessage.add msg_key message {metadata with uid=uidnext; size} >>= fun () ->
      let idx_key = MailboxIndex.create key in
      MailboxIndex.add idx_key uid

    (* remove message from the store *)
    let remove key uid = 
      let msg_key = MailboxMessage.create key uid in
      MailboxMessage.read_meta msg_key >>= function
      | `NotFound -> return ()
      | `Ok message_meta ->
        MailboxMessage.remove msg_key >>= fun () ->
        let index = MailboxIndex.create key in
        MailboxIndex.remove index uid >>= fun () ->
        update_mailbox_meta key message_meta ~adding:false ~countup:(-1)
        ~uidnextup:0 ~up:(-1) >>= fun _ -> return ()

    (* read message *)
    let read_message key ?filter seq =
      let open Interpreter in
      let index_key = MailboxIndex.create key in
      MailboxIndex.get_uid index_key seq >>= function
      | `NotFound -> return `Eof
      | `Ok uid ->
        Printf.printf "found uid %s for seq %d\n%!" uid seq;
        let msg_key = MailboxMessage.create key uid in
        MailboxMessage.read_message_and_meta msg_key >>= function
        | `NotFound -> return `NotFound (* probably get if it was deleted TBD *)
        | `Ok (message,meta) ->
          if filter = None ||
              exec_search message.email (Core.Std.Option.value_exn filter) meta seq then (
            return (`Ok (message,meta))
          ) else (
            return `NotFound
          )

    (* read metadata only *)
    let read_metadata key seq =
      let index_key = MailboxIndex.create key in
      MailboxIndex.get_uid index_key seq >>= function
      | `NotFound -> return `NotFound
      | `Ok uid ->
        let msg_key = MailboxMessage.create key uid in
        MailboxMessage.read_meta msg_key >>= function
        | `NotFound -> assert(false)
        | `Ok meta -> return (`Ok meta)

    (* update metadata *)
    let update_metadata key seq metadata =
      let idx_key = MailboxIndex.create key in
      MailboxIndex.get_uid idx_key seq >>= function
      | `Ok uid ->
        update_mailbox_meta key metadata ~adding:false ~countup:0 ~uidnextup:0 ~up:1 >>= fun _ ->
        let msg_key = MailboxMessage.create key uid in
        MailboxMessage.update_metadata msg_key metadata
      | `NotFound -> return `NotFound

    (* mailbox exists *)
    let exists t =
      Printf.printf "----------- exists %s\n%!" (IrminsuleIntf.key_to_string t);
      IrminsuleIntf.create () >>= fun s ->
      let meta_key = MailboxMetadata.create t in
      MailboxMetadata.exists meta_key >>= fun res ->
      if res = false then (
        return `No
      ) else (
        MailboxMetadata.read meta_key >>= fun mailbox_meta ->
        if mailbox_meta.folders = true then (
          return `Folder
        ) else (
          return `Storage
        )
      )

    (* create mailbox *)
    let create_mailbox t ?(folders=false)=
      let meta_key = MailboxMetadata.create t in
      MailboxMetadata.read meta_key >>= fun mailbox_meta ->
      let mailbox_meta = update_mailbox_metadata ~header:mailbox_meta ~folders () in
      MailboxMetadata.update meta_key mailbox_meta

    (* delete mailbox *)
    let delete_mailbox  t =
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.remove s t

    (* move mailbox *)
    let move_mailbox t1 t2 =
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.read_view s t1 >>= fun view ->
      IrminsuleIntf.update_view s t2 view >>= fun () ->
      IrminsuleIntf.remove s t1

    (* copy mailbox *)
    let copy_mailbox t1 t2 ~filter =
      Printf.printf "copy_mailbox\n%!";
      exists t1 >>= function
      | `No -> return `SrcNotExists
      | _ -> exists t2 >>= function
        | `No | `Folder -> return `DestNotExists
        | `Storage ->
          let open Interpreter in
          let (buid,sequence) = filter in
          IrminsuleIntf.create () >>= fun s ->
          let rec copy t1 t2 seq = 
            read_message t1 seq >>= function 
            | `Eof | `NotFound -> return ()
            | `Ok (message,metadata) ->
              (
              if exec_seq sequence seq then
                add t2 message metadata
              else
                return ()
              ) >>= fun () -> copy t1 t2 (seq + 1)
          in
          copy t1 t2 1 >>= fun () -> return `Ok
    
    (* delete all records with \Delete flag *)
    let expunge t =
      let rec delete t seq =
        read_metadata t seq >>= function
        | `NotFound -> return ()
        | `Ok meta ->
          (if (Core.Std.List.find meta.flags 
            ~f:(fun f -> if f = Flags_Deleted then true else false)) <> None then
            remove t (string_of_int meta.uid)
          else
            return ()
          ) >>= fun () -> delete t (seq + 1)
      in
      delete t 1

    (* list content of the mailbox *)
    let list_mbox s key = 
      let doprint l =
        Printf.printf "------- list_mbox %s\n%!" (IrminsuleIntf.key_to_string key);
        List.iter
        (fun k ->
          Printf.printf "key: %!" ; List.iter (fun k -> Printf.printf
          "%s/%!" k) k;
          Printf.printf "\n%!";
        ) l
      in
      let key = Core.Std.List.concat [key;["mailboxes"]] in
      IrminsuleIntf.list s [key] >>= fun l -> doprint l; return l


    (* is mailbox a folder *)
    let isfolder key = 
      let meta = MailboxMetadata.create key in
      MailboxMetadata.read meta >>= fun meta ->
        return meta.folders

    (* recursively list all mailboxes,
     * should add the starting folder and the mailbox with wildcards TBD *)
    let list_store t =
      IrminsuleIntf.create () >>= fun s ->
      let rec list_store_ s key ~init ~f =
        list_mbox s key >>= fun content ->
        Lwt_list.fold_left_s (fun (acc,cnt) name ->
          isfolder name >>= fun res ->
          if res = true then (
            list_store_ s name ~init:acc ~f >>= fun (acc,cnt) -> 
            return ((f acc (`Folder (IrminsuleIntf.key_to_path name,cnt))),cnt+1)
          ) else (
            return (f acc (`Storage (IrminsuleIntf.key_to_path name)), cnt + 1)
          )
        ) (init,0) content
      in
      list_store_ s t ~init:[] ~f:(fun acc item -> item::acc) >>= fun (acc,_) ->
        return acc

    (* search messages with the search filter *)
    let search_with t filter = 
      let open Interpreter in
      let (buid,keys) = filter in
      IrminsuleIntf.create () >>= fun s ->
      let rec doread acc seq =
        read_message t seq >>= function
        | `Eof -> return acc
        | `NotFound -> doread acc (seq + 1)
        | `Ok (message,meta) ->
          let res = exec_search message.email keys meta seq in 
          if res then
            let selected = if buid then meta.uid else seq in
            doread (selected :: acc) (seq + 1)
          else
            doread acc (seq + 1)
      in
      doread [] 1

    (* get mailbox metadata *)
    let get_mailbox_metadata t =
      let meta_key = MailboxMetadata.create t in
      MailboxMetadata.read meta_key 

    (* get subscription *)
    let get_subscription t =
      let subscr_key = MailboxesSubscription.create t in
      MailboxesSubscription.get_subscription subscr_key

    (* subscribe *)
    let subscribe t mailbox =
      let subscr_key = MailboxesSubscription.create t in
      MailboxesSubscription.subscribe subscr_key mailbox

    (* unsubscribe *)
    let unsubscribe t mailbox =
      let subscr_key = MailboxesSubscription.create t in
      MailboxesSubscription.unsubscribe subscr_key mailbox

    (* create user account *)
    let create_account t =
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.mem s t >>= fun res ->
      if res then
        return `Exists
      else
        (* create meta under user with folders set to true *)
        create_mailbox t ~folders:true >>= fun () ->
        IrminsuleIntf.update s t IrminStorageConfig.version >>= fun () -> return `Ok

    (* remove user account *)
    let remove_account t =
      Printf.printf "---- remove_account %s\n%!" (IrminsuleIntf.key_to_string t);
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.mem s t >>= fun res ->
      if res = false then (
        return `DoesntExist
      ) else
        IrminsuleIntf.remove s t >>= fun () -> 
        return `Ok

    let to_string t =
      (IrminsuleIntf.key_to_path t)

    let show_all t =
      Printf.printf "---------- mailbox messages\n%!";
      let mail_key = MailboxMessage.create t "" in
      MailboxMessage.list_messages mail_key >>= fun l ->
      Core.Std.List.iter l ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      Printf.printf "---------- mailbox index\n%!";
      let index_key = MailboxIndex.create t in
      IrminsuleIntf.create () >>= fun s ->
      MailboxIndex.get_uids s index_key >>= fun uids ->
      Core.Std.List.iter uids ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      Printf.printf "---------- mailbox metadata\n%!";
      let meta_key = MailboxMetadata.create t in
      MailboxMetadata.read meta_key >>= fun meta ->
      Printf.printf "%s\n%!" (Sexp.to_string (sexp_of_mailbox_metadata meta));
      Printf.printf "---------- subscriptions\n%!";
      let subs_key = MailboxesSubscription.create t in
      MailboxesSubscription.get_subscription subs_key >>= fun subscr ->
      Core.Std.List.iter subscr ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
        return ()
  
  end
