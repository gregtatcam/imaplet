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
open Email_message.Mailbox.Message
open StorageMeta
open Sexplib
open Irmin_unix
open ServerConfig
open Mflags

let path = srv_config.irmin_path
(*
module Git =
  IrminGit.Make(IrminKey.SHA1)(IrminContents.String)(IrminReference.String)
  module Store = (val Git.create ~bare:true ~kind:`Disk ~root:path ())
*)

module Git = IrminGit.FS(struct
  let root = Some path
  let bare = true
end)

module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)

(* simple uid cache, it will not work with multiple connections TBD !!! *)
let cache_uids = ref []
let cache_dirty = ref true
let cache_name = ref ""

let is_dirty () = !cache_dirty

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

    let view_key_to_path key = 
     let open Core.Std in
     List.foldi key 
     ~f:(fun i acc item -> 
       if i % 2 = 0 then ( (* skip mailboxes *) 
         if acc = "" then
           acc
         else
           acc ^ "/" 
       ) else 
         acc ^ item
       ) ~init:""

    let create () =
      Store.create ()

    let remove s key =
      Store.remove s key

    let list s key =
      Store.list s key

    let mem s key =
      Store.mem s key

    let update s key =
      Store.update s key

    let update_view store key view =
      Printf.printf "------ store update_view %s\n%!" (key_to_string key);
      Store.View.update_path store key view

    let read_view store key =
      Printf.printf "------ reading view %s\n%!" (key_to_string key);
      Store.View.of_path store key

    let begin_transaction key =
      create () >>= fun s ->
      Printf.printf "------ creating view %s\n%!" (key_to_string key);
      Store.View.of_path s key >>= fun v ->
      return (s,v,key,ref false)

    let end_transaction t =
      let (s,v,k,d) = t in
      if !d = true then (
        Printf.printf "++++++++++++++++++ commiting!!!\n%!";
        Store.View.update_path s k v >>= fun () ->
        d := false;
        return ()
      ) else
        return ()

    let view_key t =
      let (_,_,k,_) = t in
      k

    let view_key_to_str t =
      let (_,_,k,_) = t in
      key_to_string k

    let tr_update store key data =
      Printf.printf "------ store view.update %s\n" (key_to_string key);
      let (_,v,_,d) = store in
      Store.View.update v key data >>= fun () ->
      d := true;
      return ()

    let tr_read store key =
      let (_,v,_,_) = store in
      Store.View.read v key

    let tr_read_exn store key =
      let (_,v,_,_) = store in
      Store.View.read_exn v key

    let tr_list store key =
      Printf.printf "------ store list %s\n%!" (key_to_string (List.hd key));
      let (_,v,_,_) = store in
      Store.View.list v key

    let tr_remove store key =
      Printf.printf "------ store remove %s\n" (key_to_string key);
      let (_,v,_,d) = store in
      Store.View.remove v key >>= fun () ->
      d := true;
      return ()

    let tr_mem store key =
      let (_,v,_,_) = store in
      Store.View.mem v key

  end

exception NotFound

(* store the message broken down into postmark, header, content
 * stored under top-level/messages/uid key, where top-level is
 * imaplet:user-name:mailboxes:mbox-name:mailboxes:mbox-name1...
 * top-level/messages/uid/postmark
 * top-level/messages/uid/headers
 * top-level/messages/uid/content
 * top-level/messages/uid/meta - per message metadata like flags, uid, etc
 *)
module MailboxMessage =
  struct
    let mk_key t key_type = 
      let append t key =
        let (v,uid,mailbox) = t in
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
    let create v mailbox uid = 
      (v, uid, ["messages"])

    (* add message and message metadata *)
    let add t (message:Mailbox.Message.t) message_metadata =
      Printf.printf "adding messge with uid %d\n%!" message_metadata.uid;
      let (v,_,_) = t in
      (*let message = Mailbox.Message.t_of_sexp (Sexp.of_string message) in*)
      let postmark = message.postmark in
      let postmark_sexp = Mailbox.Postmark.sexp_of_t postmark in
      let email = message.email in
      let email_sexp = Email.sexp_of_t email in
      let header = Email.header email in
      let header_sexp = Header.sexp_of_t header in
      let update v t key_type str =
        let key = mk_key t key_type in
        IrminsuleIntf.tr_update v key str
      in
      update v t `Postmark (Sexp.to_string postmark_sexp) >>= fun () ->
      update v t `Headers (Sexp.to_string header_sexp) >>= fun () ->
      (* should break email into separate header and content sexp TBD *)
      update v t `Email (Sexp.to_string email_sexp) >>= fun () ->
      let sexp = sexp_of_mailbox_message_metadata message_metadata in
      update v t `Meta (Sexp.to_string sexp)

    (* remove the message *)
    let remove t =
      let (v,_,_) = t in
      Lwt_list.iter_s (fun k -> let key = mk_key t k in IrminsuleIntf.tr_remove v key)
      [`Headers;`Email;`Meta; `Postmark; `Root;]

    (* read message *)
    let read_message t =
      let (v,_,_) = t in
      try
        let read v t key_type =
          let key = mk_key t key_type in
          IrminsuleIntf.tr_read v key >>= function
          | Some value -> return value
          | None -> Printf.printf "key not found%s\n%!" (IrminsuleIntf.key_to_string key ); raise NotFound
        in
        read v t `Postmark >>= fun postmark_sexp_str ->
        read v t `Email >>= fun email_str ->
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
    let read_meta ?filter t =
      let open Interpreter in
      let (v,_,_) = t in
      let key = mk_key t `Meta in
      IrminsuleIntf.tr_read v key >>= function
      | Some str ->
        let sexp = Sexp.of_string str in
        let metadata = mailbox_message_metadata_of_sexp sexp in
        if filter = None then
          return (`Ok metadata)
        else (
          let (id,filter) = Core.Std.Option.value_exn filter in
          if exec_seq filter id then
            return (`Ok metadata)
          else
            return `NotFound
        )
      | None -> return `NotFound

    let read_message_and_meta t =
      read_message t >>= function
      | `NotFound -> return `NotFound
      | `Ok message -> read_meta t >>= function
        | `NotFound -> assert(false)
        | `Ok meta -> return (`Ok (message, meta))

    (* update message metadata *)
    let update_metadata t metadata =
      let (v,_,_) = t in
      let key = mk_key t `Meta in
      IrminsuleIntf.tr_mem v key >>= fun res ->
      if res = false then (
        Printf.printf "update_metadata key not found\n%!";
        return `NotFound
      ) else
        let sexp = sexp_of_mailbox_message_metadata metadata in
        Printf.printf "update_metadata %s\n%!" (Sexp.to_string sexp);
        IrminsuleIntf.tr_update v key (Sexp.to_string sexp) >>= fun () ->
        return `Ok

    (* list uid of messages *)
    let list_messages t =
      let list_subtr v k =
        IrminsuleIntf.tr_list v [k] >>= fun l ->
        return (Core.Std.List.fold l ~init:"" ~f:(fun acc i ->
          acc ^ ":" ^ (Core.Std.List.last_exn i)
        ))
      in
      let (v,_,m) = t in
      IrminsuleIntf.tr_list v [m] >>= fun l ->
      Lwt_list.fold_left_s (fun acc i ->
        list_subtr v i >>= fun s ->
        return (((IrminsuleIntf.key_to_string i) ^ ":" ^ s) :: acc)
      ) [] l

  end

(* index for message sequence number
 * the key mailbox/index/sequence/uids
 * for now just a simple list, need to change TBD *)
module MailboxIndex =
  struct
    (* mailbox has all key parts including the name of the mailbox *)
    let create v mailbox =
      let str = (IrminsuleIntf.key_to_string mailbox) in
      (if !cache_name <> str then
        cache_dirty := true);
      cache_name := str;
      v,str,["index";"sequence";"uids"]

    (* update the complete index list *)
    let update_uids t uids =
      cache_dirty := true;
      let (v,_,k) = t in
      let sexp = Core.Std.List.sexp_of_t (fun i -> Sexp.of_string i) uids in
      IrminsuleIntf.tr_update v k (Sexp.to_string sexp) 

    (* get complete index list *)
    let get_uids t =
      let (v,_,k) = t in
      if is_dirty () = false then
        return !cache_uids
      else (
        IrminsuleIntf.tr_read v k >>= fun res ->
        (match res with 
        | Some str ->
          let sexp = Sexp.of_string str in
          return (Core.Std.List.t_of_sexp (fun i -> Sexp.to_string i) sexp)
        | None -> update_uids t [] >>= fun () -> return []) >>= fun uids ->
        cache_uids := uids;
        cache_dirty := false;
        return uids
      )

    (* add uid to the index list *)
    let add t uid =
      cache_dirty := true;
      get_uids t >>= fun uids ->
      update_uids t (Core.Std.List.concat [uids;[uid]])

    (* remove uid from the index list *)
    let remove t uid = 
      cache_dirty := true;
      get_uids t >>= fun uids ->
      update_uids t (Core.Std.List.fold_right uids ~init:[] 
      ~f:(fun i acc -> 
        if i = uid then 
          acc
        else
          i :: acc
      ))

    (* find uid corresponding to the sequence *)
    let get_uid t seq =
      assert(seq > 0);
      let (v,_,k) = t in
      let seq = seq - 1 in (* record is 1 based, index is 0 based *)
      get_uids t >>= fun uids ->
      match (Core.Std.List.nth uids seq) with
      | Some uid -> return (`Ok uid)
      | None -> Printf.printf "no uids found %s %d\n%!"
      (IrminsuleIntf.key_to_string k) seq; return `NotFound

    (* find sequence corresponding to uid *)
    let get_seq t uid =
      get_uids t >>= fun uids ->
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
    let create v ?(path=[]) () =
      v,List.concat [path;["meta"]]

    (* update the metadata *)
    let update t meta =
      Printf.printf "------------- updating metadata %s %b\n%!" meta.uidvalidity meta.folders;
      let (v,k) = t in
      let sexp = sexp_of_mailbox_metadata meta in
      IrminsuleIntf.tr_update v k (Sexp.to_string sexp)

    (* get the metadata *)
    let read t =
      let (v,k) = t in
      IrminsuleIntf.tr_read v k >>= function
      | Some str ->
        let sexp = Sexp.of_string str in
        return (mailbox_metadata_of_sexp sexp)
      | None ->
        let meta = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity()) () in
        update t meta >>= fun () ->
        return meta

    (* does the meta key exists?
     * it indicates if the mailbox exists or not
     *)
    let exists t =
      let (v,k) = t in
      IrminsuleIntf.tr_mem v k

  end

(* mailboxes subscription *)
module MailboxesSubscription =
  struct

    (* create type *)
    let create v =
      v,["subscription"]

    (* convert the list to a string of sexp *)
    let str_sexp_of_list l =
      let sexp = Core.Std.List.sexp_of_t (fun i -> Sexp.of_string i) l in
      Sexp.to_string sexp

    (* convert string of sexp to the list *)
    let list_of_str_sexp str =
      let sexp = Sexp.of_string str in
      (Core.Std.List.t_of_sexp (fun i -> Sexp.to_string i) sexp)

    (* update subscription list *)
    let update t l =
      let (v,k) = t in
      let str = str_sexp_of_list l in
      IrminsuleIntf.tr_update v k str

    (* read subscription *)
    let read t =
      let (v,k) = t in
      IrminsuleIntf.tr_read v k >>= function
      | Some str -> return (list_of_str_sexp str)
      | None -> return []

    (* subscribe *)
    let subscribe t mbox =
      read t >>= fun l ->
      if (Core.Std.List.find l ~f:(fun i -> if i = mbox then true else false) <> None) then 
        return ()
      else
        update t [mbox]

    (* unsubscribe *)
    let unsubscribe t mbox =
      read t >>= fun l ->
      update t [mbox]

    (* get subscription *)
    let get_subscription t =
      read t 
  end

module UserAccount = 
  struct
    (* create type *)
    let create t = t

    (* create new account *)
    let create_account t =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let meta_key = MailboxMetadata.create v () in
      MailboxMetadata.exists meta_key >>= fun res ->
      (if res then
        return `Exists
      else
        let meta = empty_mailbox_metadata () in
        let meta = {meta with folders = true} in
        MailboxMetadata.update meta_key meta >>= fun () -> 
        return `Ok
      ) >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return res

    (* remove account *)
    let remove_account t =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let meta_key = MailboxMetadata.create v () in
      MailboxMetadata.exists meta_key >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      if res = false then
        return `DoesntExist
      else
        IrminsuleIntf.create () >>= fun s ->
        IrminsuleIntf.remove s t >>= fun () ->
        return `Ok

  end

type position = [`Position of int|`UID of int]

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
    val read_message : t -> ?filter:(States.searchKey) States.searchKeys ->
      position -> [`Ok of (Mailbox.Message.t *
    mailbox_message_metadata)| `NotFound|`Eof] Lwt.t

    (* read metadata only *)
    val read_metadata : t -> ?filter:States.sequence -> position -> [`Ok of mailbox_message_metadata| `Eof|`NotFound] Lwt.t

    (* update metadata *)
    val update_metadata : t -> position -> mailbox_message_metadata ->
      [`Ok|`Eof|`NotFound] Lwt.t

    (* create the mailbox *)
    val create_mailbox : t -> ?folders:bool -> unit -> unit Lwt.t

    (* delete the mailbox *)
    val delete_mailbox : t -> unit Lwt.t

    (* move the mailbox *)
    val move_mailbox : t -> t -> unit Lwt.t

    (* copy the mailbox *)
    val copy_mailbox : t -> t -> filter:(bool*States.sequence) -> [`Ok|`SrcNotExists|`DestNotExists] Lwt.t

    (* expunge deleted messages *)
    val expunge : t -> int list Lwt.t

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

(* top level key is, for instance /Test/Test1
 imaplet;user-name;mailboxes/Test/mailboxes/Test1
*)
module IrminMailbox : IrminMailbox_intf with type t = string list =
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
    let update_mailbox_meta v message_meta ~adding ~countup ~uidnextup ~up =
      let meta_key = MailboxMetadata.create v () in
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

    let _add v key message metadata =
      update_mailbox_meta v metadata ~adding:true ~countup:1 ~uidnextup:1 ~up:1 >>= fun uidnext ->
      let uid = string_of_int uidnext in
      let msg_key = MailboxMessage.create v key uid in
      (*let message = Sexp.to_string (Mailbox.Message.sexp_of_t message) in *)
      let size = String.length (Mailbox.Message.to_string message) in
      MailboxMessage.add msg_key message {metadata with uid=uidnext; size} >>= fun () ->
      let idx_key = MailboxIndex.create v key in
      MailboxIndex.add idx_key uid 

    (* add message and message metadata to the store *)
    let add key message metadata =
      IrminsuleIntf.begin_transaction key >>= fun v ->
      _add v key message metadata >>= fun () ->
      IrminsuleIntf.end_transaction v

    let _remove v key uid = 
      let msg_key = MailboxMessage.create v key uid in
      MailboxMessage.read_meta msg_key >>= function
      | `NotFound -> Printf.printf "_remove, uid not found %s\n%!" uid; return ()
      | `Ok message_meta ->
        MailboxMessage.remove msg_key >>= fun () ->
        let index = MailboxIndex.create v key in
        MailboxIndex.remove index uid >>= fun () ->
        update_mailbox_meta v message_meta ~adding:false ~countup:(-1)
        ~uidnextup:0 ~up:(-1) >>= fun _ -> return ()

    (* remove message from the store *)
    let remove key uid = 
      IrminsuleIntf.begin_transaction key >>= fun v ->
      _remove v key uid >>= fun () ->
      IrminsuleIntf.end_transaction v

    let get_uid v key seq =
      match seq with 
      | `UID uid -> return (`Ok (string_of_int uid))
      | `Position seq ->
        let index_key = MailboxIndex.create v key in
        MailboxIndex.get_uid index_key seq 

    let get_pos = function
      | `UID u -> u
      | `Position p -> p

    let _read_message v key ?filter seq =
      let open Interpreter in
      get_uid v key seq >>= function
      | `NotFound -> return `Eof
      | `Ok uid ->
        let msg_key = MailboxMessage.create v key uid in
        MailboxMessage.read_message_and_meta msg_key >>= function
        | `NotFound -> return `NotFound 
        | `Ok (message,meta) ->
          if filter = None ||
              exec_search message.email (Core.Std.Option.value_exn filter) meta (get_pos seq) then (
            return (`Ok (message,meta))
          ) else (
            return `NotFound
          )

    (* read message *)
    let read_message key ?filter seq =
      IrminsuleIntf.begin_transaction key >>= fun v ->
      _read_message v key ?filter seq >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () -> return res

    let _read_metadata v key ?filter seq =
      get_uid v key seq >>= function
      | `NotFound -> return `Eof
      | `Ok uid ->
        let seq = match seq with |`UID seq -> seq | `Position seq -> seq in
        let msg_key = MailboxMessage.create v key uid in
        let f = if filter = None then None else Some (seq,Core.Std.Option.value_exn filter) in
        MailboxMessage.read_meta ?filter:f msg_key >>= function
        | `NotFound -> return `NotFound
        | `Ok meta -> return (`Ok meta)

    (* read metadata only *)
    let read_metadata key ?filter seq =
      IrminsuleIntf.begin_transaction key >>= fun v ->
      _read_metadata v key ?filter seq >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return res

    (* update metadata *)
    let update_metadata key seq metadata =
      let end_tr v r =
        IrminsuleIntf.end_transaction v >>= fun () -> return r
      in
      IrminsuleIntf.begin_transaction key >>= fun v ->
      get_uid v key seq >>= function
      | `Ok uid ->
        update_mailbox_meta v metadata ~adding:false ~countup:0 ~uidnextup:0 ~up:1 >>= fun _ ->
        let msg_key = MailboxMessage.create v key uid in
        MailboxMessage.update_metadata msg_key metadata >>= fun r -> end_tr v r 
      | `NotFound -> end_tr v `Eof

    (* mailbox exists *)
    let exists t =
      let end_tr v r =
        IrminsuleIntf.end_transaction v >>= fun () -> return r
      in
      Printf.printf "----------- exists %s\n%!" (IrminsuleIntf.key_to_string t);
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let meta_key = MailboxMetadata.create v () in
      MailboxMetadata.exists meta_key >>= fun res ->
      if res = false then (
        end_tr v `No
      ) else (
        MailboxMetadata.read meta_key >>= fun mailbox_meta ->
        if mailbox_meta.folders = true then (
          end_tr v `Folder
        ) else (
          end_tr v `Storage
        )
      )

    let _create_mailbox v ?(folders=false) ()=
      let meta_key = MailboxMetadata.create v () in
      MailboxMetadata.read meta_key >>= fun mailbox_meta ->
      let mailbox_meta = update_mailbox_metadata ~header:mailbox_meta ~folders () in
      Printf.printf "creating mailbox with folders set to %b\n%!"
      mailbox_meta.folders;
      MailboxMetadata.update meta_key mailbox_meta 

    (* create mailbox *)
    let create_mailbox t ?(folders=false) () =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      _create_mailbox v ~folders () >>= fun () ->
      IrminsuleIntf.end_transaction v

    (* delete mailbox *)
    let delete_mailbox  t =
      IrminsuleIntf.create () >>= fun s ->
      IrminsuleIntf.remove s t

    (* move mailbox transaction - TBD *)
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
          IrminsuleIntf.begin_transaction t1 >>= fun v1 ->
          IrminsuleIntf.begin_transaction t2 >>= fun v2 ->
          let rec copy v1 t1 v2 t2 seq = 
            _read_message v1 t1 (`Position seq) >>= function 
            | `Eof | `NotFound -> return ()
            | `Ok (message,metadata) ->
              (
              let id = if buid then metadata.uid else seq in
              if exec_seq sequence id then
                _add v2 t2 message metadata
              else
                return ()
              ) >>= fun () -> copy v1 t1 v2 t2 (seq + 1)
          in
          copy v1 t1 v2 t2 1 >>= fun () -> 
          IrminsuleIntf.end_transaction v1 >>= fun () ->
          IrminsuleIntf.end_transaction v2 >>= fun () ->
          return `Ok
    
    (* delete all records with \Delete flag *)
    let expunge t =
      (*
      let rec delete v t acc seq =
        _read_metadata v t (`Position seq) >>= function
        | `NotFound | `Eof -> return acc
        | `Ok meta ->
          (if (Core.Std.List.find meta.flags 
            ~f:(fun f -> if f = Flags_Deleted then true else false)) <> None then
            _remove v t (string_of_int meta.uid) >>= fun() -> 
            return (meta.uid :: acc)
          else
            return acc
          ) >>= fun acc -> delete v t acc (seq + 1)
      in
      IrminsuleIntf.begin_transaction t >>= fun v ->
      delete v t [] 1 >>= fun acc ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return acc
      *)
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let index_key = MailboxIndex.create v t in
      MailboxIndex.get_uids index_key >>= fun uids ->
      Lwt_list.fold_left_s (fun acc uid ->
        _read_metadata v t (`UID (int_of_string uid)) >>= function
        | `NotFound | `Eof -> return acc
        | `Ok meta ->
            if (Core.Std.List.find meta.flags 
            ~f:(fun f -> if f = Flags_Deleted then true else false)) <> None then
            _remove v t (string_of_int meta.uid) >>= fun() -> 
            return (meta.uid :: acc)
          else
            return acc
      ) [] uids >>= fun acc ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return acc

    (* list content of the mailbox *)
    let list_mbox v key = 
      (*let doprint l = 
        List.iter
        (fun k ->
          Printf.printf "key: %!" ; List.iter (fun k -> Printf.printf
          "%s/%!" k) k;
          Printf.printf "\n%!";
        ) l
      in*)
      let key = Core.Std.List.concat [key;["mailboxes"]] in
      IrminsuleIntf.tr_list v [key] >>= fun l -> return l


    (* is mailbox a folder *)
    let isfolder v path = 
      let meta_key = MailboxMetadata.create v ~path () in
      MailboxMetadata.read meta_key >>= fun meta ->
      return meta.folders

    (* recursively list all mailboxes,
     * should add the starting folder and the mailbox with wildcards TBD *)
    let list_store t =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let rec list_store_ v key ~init ~f =
        list_mbox v key >>= fun content ->
        Lwt_list.fold_left_s (fun (acc,cnt) name ->
          isfolder v name >>= fun res ->
          if res = true then (
            list_store_ v name ~init:acc ~f >>= fun (acc,cnt) -> 
            return ((f acc (`Folder (IrminsuleIntf.view_key_to_path name,cnt))),cnt+1)
          ) else (
            return (f acc (`Storage (IrminsuleIntf.view_key_to_path name)), cnt + 1)
          )
        ) (init,0) content
      in
      list_store_ v [] ~init:[] ~f:(fun acc item -> item::acc) >>= fun (acc,_) ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return acc

    (* search messages with the search filter *)
    let search_with t filter = 
      let open Interpreter in
      let (buid,keys) = filter in
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let rec doread acc seq =
        _read_message v t (`Position seq) >>= function
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
      doread [] 1 >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return res

    (* get mailbox metadata *)
    let get_mailbox_metadata t =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let meta_key = MailboxMetadata.create v () in
      MailboxMetadata.read meta_key >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return res

    (* get subscription *)
    let get_subscription t =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let subscr_key = MailboxesSubscription.create v in
      MailboxesSubscription.get_subscription subscr_key >>= fun res ->
      IrminsuleIntf.end_transaction v >>= fun () ->
      return res

    (* subscribe *)
    let subscribe t mailbox =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let subscr_key = MailboxesSubscription.create v in
      MailboxesSubscription.subscribe subscr_key mailbox >>= fun () ->
      IrminsuleIntf.end_transaction v 

    (* unsubscribe *)
    let unsubscribe t mailbox =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      let subscr_key = MailboxesSubscription.create v in
      MailboxesSubscription.unsubscribe subscr_key mailbox >>= fun () ->
      IrminsuleIntf.end_transaction v 

    (* create user account *)
    let create_account t =
      let acct_key = UserAccount.create t in
      UserAccount.create_account acct_key

    (* remove user account *)
    let remove_account t =
      let acct_key = UserAccount.create t in
      UserAccount.remove_account acct_key

    let to_string t =
      (IrminsuleIntf.key_to_path t)

    let show_all t =
      IrminsuleIntf.begin_transaction t >>= fun v ->
      Printf.printf "---------- mailbox messages\n%!";
      let mail_key = MailboxMessage.create v t "" in
      MailboxMessage.list_messages mail_key >>= fun l ->
      Core.Std.List.iter l ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      Printf.printf "---------- mailbox index\n%!";
      let index_key = MailboxIndex.create v t in
      MailboxIndex.get_uids index_key >>= fun uids ->
      Core.Std.List.iter uids ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      Printf.printf "---------- mailbox metadata\n%!";
      let meta_key = MailboxMetadata.create v () in
      MailboxMetadata.read meta_key >>= fun meta ->
      Printf.printf "%s\n%!" (Sexp.to_string (sexp_of_mailbox_metadata meta));
      Printf.printf "---------- subscriptions\n%!";
      let subs_key = MailboxesSubscription.create v in
      MailboxesSubscription.get_subscription subs_key >>= fun subscr ->
      Core.Std.List.iter subscr ~f:(fun i -> Printf.printf "%s %!" i); Printf.printf "\n%!";
      IrminsuleIntf.end_transaction v >>= fun () -> return ()
  
  end
