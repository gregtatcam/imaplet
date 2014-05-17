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
open Primitives
open Storage
open StorageMeta
open IrminStorageCmd
open Email_message
open Sexplib

exception Mismatched_arg of string * string

let what_response = function
  | `Bootstrap -> "boostrap"
  | `Copy_with _ -> "copy_with"
  | `Create _ -> "create"
  | `Create_account _ -> "create_account"
  | `Delete  -> "delete"
  | `Eof -> "eof"
  | `Exists _ -> "exists"
  | `Expunge -> "expunge"
  | `Get_subscription _ -> "get_subscription"
  | `List_store _ -> "list_store"
  | `Mailbox_metadata _ -> "mailbox_metadata"
  | `Move -> "move" 
  | `Reader _ -> "reader"
  | `Reader_metadata _ -> "reader_metadata"
  | `Remove_account _ -> "remove_account"
  | `Rebuild_index -> "rebuild_index"
  | `Search_with _ -> "search_with"
  | `Subscribe -> "subscribe"
  | `Unsubscribe -> "unsubscribe"
  | `Update_index _ -> "update_index"
  | `Writer _ -> "writer"
  | `Writer_metadata _ -> "writer_metadata"
  | _ -> "unknown"

let raise_with expected resp =
  raise(Mismatched_arg (("*************** " ^ expected),(what_response resp) ))

let marshal a =
  Marshal.to_string a [Marshal.Compat_32]

(* this is net client for the irmin server, just send/receive *)
let request r w req =
  Writer.write w (marshal (Sexp.to_string (sexp_of_irminRequest req))); Writer.flushed w >>= fun () ->
  Reader.read_marshal r >>= function
  | `Ok buff -> printf "response\n%!";
      return (irminResponse_of_sexp (Sexp.of_string buff))
  | `Eof -> printf "eof\n%!"; return `Eof

module MakeIrminsuleAccessor 
    (P':Position) 
    (L':Location) :
    MailboxAccessor_intf with 
      type a = string*L'.t*Reader.t * Writer.t and 
      type t = string*L'.t*Reader.t * Writer.t =
  struct
    type a = string*L'.t*Reader.t * Writer.t
    type t = string*L'.t*Reader.t * Writer.t

    (* create accessor *)
    let create a = a

    let message () =
      let postmark = 
        Mailbox.Postmark.of_string "From dovecot@localhost.local Thu Mar 20 08:01:17 2014" in
      let email = 
        Email.of_string "Subject: test\n\ntest\n\n" in
      {Mailbox.Message.email=email;Mailbox.Message.postmark=postmark}

    (* read block from the storage at requested position,
     *)
    let reader tp ?filter pos =
      let (u,l,r,w) = tp in
      request r w (`Reader (u,L'.to_str l,pos,filter)) >>= fun resp ->
      match resp with
      | `Reader data -> return data
      | _ -> raise_with "reader" resp 

    (* read the metadata only *)
    let reader_metadata tp pos = 
      let (u,l,r,w) = tp in
      request r w (`Reader_metadata (u,L'.to_str l,pos))>>= fun resp ->
      match resp with
      | `Reader_metadata data -> return data
      | _ -> raise_with"reader_metadata" resp

     (* Can only do append so Position is not applicable, nor is the header
      need to write back index header or accumulate header and have the
      fold handle cumulative header
     *)
    let writer tp pos blk = 
      let (u,l,r,w) = tp in
      request r w (`Writer (u,L'.to_str l,pos,blk)) >>= fun resp ->
      match resp with
      | `Writer data -> return data
      | _ -> raise_with"writer" resp

    (* write the metadata (flags only) *)
    let writer_metadata tp metadata pos =
      let (u,l,r,w) = tp in
      request r w (`Writer_metadata (u,L'.to_str l,pos,metadata))>>= fun resp ->
      match resp with
      | `Writer_metadata data -> return data
      | _ -> raise_with"writer_metadata" resp
  end

module IrminsuleStorageAccessor = MakeIrminsuleAccessor
  (BasicPosition)
  (BasicLocation)

module MakeIrminsuleStorage 
  (L':Location) 
  (MBXAC':MailboxAccessor_intf with type a = string*L'.t*Reader.t*Writer.t and type t =
    string*L'.t*Reader.t*Writer.t) :
  MailboxStorage_intf with type loc = L'.t and type param = string*Reader.t * Writer.t and type t =
    string*L'.t*L'.t*L'.t*Reader.t*Writer.t and type accs = MBXAC'.t =
  struct
    type loc = L'.t
    type param = string*Reader.t * Writer.t
    type t = string*L'.t * L'.t * L'.t * Reader.t * Writer.t
    type accs = MBXAC'.t

    let accessor_factory a =
      build_accs_inst (module MBXAC')  a

    (* create storage type; takes location of the mailbox as a client sees it;
     * creates specific implementation location
     *)
    let create_st lc ~dirs param =
      let (mbox,inbox) = dirs in
      let (u,r,w) = param in
      (u,lc,mbox,inbox,r,w)

    let get_loc tp =
      let (u,l,_,_,_,_) = tp in l

    (* check if storage exists 
     * assume that index exists 
     *)
    let exists tp = 
      let (u,l,_,_,r,w) = tp in 
      request r w (`Exists (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `Exists data -> return data
      | _ -> raise_with"exists" resp

    (* create storage *)
    let create tp = 
      let (u,l,_,_,r,w) = tp in
      printf "irminStoreClnt create %s %s\n%!" u (L'.to_str l);
      request r w (`Create (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `Create data -> return data
      | _ -> raise_with"create" resp

    (* move storage *)
    let move tp1 tp2 = 
      let (u,l1,_,_,r,w) = tp1 in
      let (u,l2,_,_,r,w) = tp2 in
      request r w (`Move (u,L'.to_str l1,L'.to_str l2))>>= fun resp ->
      match resp with
      | `Move -> return ()
      | _ -> raise_with"move" resp

    (* delete storage *)
    let delete tp = 
      let (u,l,_,_,r,w) = tp in
      request r w (`Delete (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `Delete -> return ()
      | _ -> raise_with"delete" resp

    (* update index from the mailbox *)
    let update_index tp = 
      let (u,l,_,_,r,w) = tp in
      request r w (`Update_index (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `Update_index data -> return data
      | _ -> raise_with"update_index" resp

    (* rebuild entier index for the mailbox *)
    let rebuild_index tp = 
      let (u,l,_,_,r,w) = tp in
      request r w (`Rebuild_index (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `Rebuild_index -> return ()
      | _ -> raise_with"rebuild_index" resp

    (* fold over index then over mailbox *)
    let fold tp ?(exclusive=false) ~init ~f = 
      let (u,l,_,_,r,w) = tp in
      let mbx_accs = accessor_factory (u,l, r, w) in
      f init mbx_accs

    (* list storage *)
    let list_store tp ~init ~f = 
      let (u,l,_,_,r,w) = tp in
      request r w (`List_store (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `List_store l ->
        (Deferred.List.fold l ~init ~f)
      | _ -> raise_with"list_store" resp

    (* copy storage with filter *)
    let copy tp1 tp2 ~f = 
      let (u,l1,_,_,r,w) = tp1 in
      let (u,l2,_,_,r,w) = tp2 in
      exists tp1 >>= function
      | `No -> return `SrcNotExists
      | _ -> exists tp2 >>= function
        | `No | `Folder -> return `DestNotExists
        | `Storage ->
          let accs_src = accessor_factory (u,l1,r,w) in
          let accs_dst = accessor_factory (u,l2,r,w) in
          let (module AccessorSrc:StorageAccessor_inst) = accs_src in
          let (module AccessorDst:StorageAccessor_inst) = accs_dst in
          let rec docopy f pos =
            AccessorSrc.StorageAccessor.reader AccessorSrc.this (`Position pos) >>= function
            | `Eof -> return ()
            | `NotFound -> docopy f (pos+1)
            | `Ok blk -> 
              if f pos blk = true then
                AccessorDst.StorageAccessor.writer AccessorDst.this `Append blk >>= fun _ ->
                docopy f (pos+1)
              else
                docopy f (pos+1)
          in
          docopy f 1 >>= fun () -> return `Ok
    
    let copy_with tp1 tp2 ~filter =
      let (u,l1,_,_,r,w) = tp1 in
      let (u,l2,_,_,r,w) = tp2 in
      request r w (`Copy_with (u,L'.to_str l1,L'.to_str l2,filter)) >>= fun resp ->
      match resp with
      | `Copy_with data -> return data
      | _ -> raise_with"copy_with" resp

    let search_with t ~filter = 
      let (u,l1,_,_,r,w) = t in
      request r w (`Search_with (u,L'.to_str l1,filter)) >>= fun resp ->
      match resp with
      | `Search_with data -> return data
      | _ -> raise_with"search_with" resp

    (* need to call expunge on each message and call f if expunged *)
    let expunge tp1 ?tmp:tp2 ~f =
      assert(tp2 <> None);
      let (u,l1,_,_,r,w) = tp1 in
      request r w (`Expunge (u,L'.to_str l1)) >>= fun resp ->
      match resp with
      | `Expunge -> return ()
      | _ -> raise_with"expunge" resp

    (* get mailbox metadata like uidvalidity and some stats *)
    let get_mailbox_metadata tp =
      let (u,l,_,_,r,w) = tp in
      request r w (`Mailbox_metadata (u,L'.to_str l)) >>= fun resp ->
      match resp with
      | `Mailbox_metadata data -> return data
      | _ -> raise_with"mailbox_metadata" resp

    (* get list of subscribed mailboxes *)
    let get_subscription tp =
      let (u,_,_,_,r,w) = tp in
      request r w (`Get_subscription u) >>= fun resp ->
      match resp with
      | `Get_subscription data -> return data
      | _ -> raise_with"get_subscription" resp

    (* subscribe the mailbox *)
    let subscribe tp = 
      let (u,l,_,_,r,w) = tp in
      request r w (`Subscribe (u,L'.to_str l))>>= fun resp ->
      match resp with
      | `Subscribe -> return ()
      | _ -> raise_with"subscribe" resp

    (* unsubscribe the mailbox *)
    let unsubscribe tp = 
      let (u,l,_,_,r,w) = tp in
      request r w (`Unsubscribe (u,L'.to_str l))>>= fun resp ->
      match resp with
      | `Unsubscribe -> return ()
      | _ -> raise_with"unsubscribe" resp

    let create_account tp =
      let (u,_,_,_,r,w) = tp in
      request r w (`Create_account u)>>= fun resp ->
      match resp with
      | `Create_account res -> return res
      | _ -> raise_with"create_account" resp

    let remove_account tp =
      let (u,_,_,_,r,w) = tp in
      request r w (`Remove_account u)>>= fun resp ->
      match resp with
      | `Remove_account res -> return res
      | _ -> raise_with"remove_account" resp

  end

module IrminsuleStorage = MakeIrminsuleStorage 
  (BasicLocation)
  (IrminsuleStorageAccessor)
