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
open Email_message
open Mflags
open StorageMeta

module type StorageAccessor_intf = 
  sig
    type a
    type t
    type blk

    (* create accessor *)
    val create : a -> t 

    (* read block from the storage at requested position,
     * if position is none then sequential access,
     * position 0 is the header if applicable `Header|`Record of int > 0
     *)
    val reader : t -> [`Position of int] -> 
      [`Ok of blk|`Eof] Deferred.t

    (* write block to the storage at requested position, 
     * If `Eof then requested position exceeds storage size
     * of rec * int
     *)
    val writer : t -> [`Append|`Position of int] -> blk ->
      [`Ok|`Eof] Deferred.t
  end

module type MboxIndexStorageAccessor_intf =
  sig
    include StorageAccessor_intf with type blk := mbox_index

    val reader_header : t -> mailbox_metadata Deferred.t

    val reader_record : t -> [`Position of int] -> 
      [`Ok of mbox_message_metadata|`Eof] Deferred.t

    val writer_header : t -> mailbox_metadata -> unit Deferred.t

    val writer_record : t -> [`Append|`Position of int] -> mbox_message_metadata -> [`Ok|`Eof] Deferred.t
  end

type mailbox_data = Mailbox.Message.t * mailbox_message_metadata

module type MailboxAccessor_intf = 
  sig
    include StorageAccessor_intf with type blk := mailbox_data

    val reader : t -> ?filter:(States.searchKey) States.searchKeys -> 
      [`Position of int] -> [`Ok of mailbox_data|`Eof|`NotFound] Deferred.t

    val writer : t -> [`Append] -> mailbox_data -> [`Ok] Deferred.t

    val reader_metadata : t -> [`Position of int] ->
      [`Ok of mailbox_message_metadata|`Eof] Deferred.t

    val writer_metadata : t -> mailbox_message_metadata -> [`Position of int] -> [`Ok|`Eof] Deferred.t
  end

module type StorageAccessor_inst = 
  sig
    module StorageAccessor : MailboxAccessor_intf
    val this : StorageAccessor.t
  end

let build_accs_inst
  (type a)
  (module SA : MailboxAccessor_intf with type a = a and type t = a)
  desc
  =
  (module struct
    module StorageAccessor = SA
    let this = SA.create desc
    end : StorageAccessor_inst
  )

module type LocationConstructor_intf =
  sig
    type t

    val construct : t -> dirs:(t*t) -> [`Index|`Mailbox] -> t

    val to_string : t -> string
  end

module type Storage_intf =
  sig
    type loc
    type param
    type t
    type accs
    type blk

    (* create storage type; takes location of the mailbox as a client sees it;
     * creates specific implementation location
     *)
    val create_st : loc -> dirs:(loc*loc) -> param -> t

    (* check if storage exists *)
    val exists : t -> [`No|`Folder|`Storage] Deferred.t

    (* create storage *)
    val create : t -> [`Folder|`Storage] Deferred.t

    (* move storage *)
    val move : t -> t -> unit Deferred.t

    (* delete storage *)
    val delete : t -> unit Deferred.t

    (* read/update storage *)
    val fold : t -> ?exclusive:bool -> init:'a -> f:('a -> accs -> 'a Deferred.t) -> 'a Deferred.t

    (* list storage *)
    val list_store : t -> init:'a ->
      f:('a -> [`Folder of string*int|`Storage of string] -> 'a Deferred.t) -> 'a Deferred.t

    (* copy storage with filter *)
    val copy : t -> t -> f:(int -> blk -> bool) -> [`Ok|`SrcNotExists|`DestNotExists] Deferred.t

  end

module type MboxIndexStorage_intf = Storage_intf with type blk := mbox_index

module type MailboxStorage_intf = 
  sig
    include Storage_intf with 
      type blk := mailbox_data

    (* read/update storage, overwriting *)
    val fold : t -> ?exclusive:bool -> init:'a -> f:('a -> (module StorageAccessor_inst) -> 'a Deferred.t) -> 'a Deferred.t

    (* copy filtered *)
    val copy_with : t -> t -> filter:(bool*States.sequence) -> [`Ok|`SrcNotExists|`DestNotExists] Deferred.t

    (* copy filtered *)
    val search_with : t -> filter:(bool*(States.searchKey) States.searchKeys) -> int list Deferred.t

    (* expunge messages with \Deleted flag *)
    val expunge : t -> ?tmp:t -> f:(int -> unit) -> unit Deferred.t

    (* update index from the mailbox *)
    val update_index : t -> [`NotExists|`Ok] Deferred.t

    (* rebuild the whole index *)
    val rebuild_index : t -> unit Deferred.t

    (* get mailbox metadata *)
    val get_mailbox_metadata : t -> mailbox_metadata Deferred.t

    (* get list of subscribed mailboxes *)
    val get_subscription : t -> string list Deferred.t

    (* subscribe the mailbox *)
    val subscribe : t -> unit Deferred.t

    (* unsubscribe the mailbox *)
    val unsubscribe : t -> unit Deferred.t

    (* create user account *)
    val create_account : t -> [`Ok|`Exists] Deferred.t

    (* remove user account *)
    val remove_account : t -> [`Ok|`DoesntExist] Deferred.t
  end


module type Storage_inst = 
  sig
    module MailboxStorage : MailboxStorage_intf
    val this : MailboxStorage.t
    val this1 : MailboxStorage.t option
  end
(*
let build_strg_inst
  (type l)
  (type p)
  (type a)
  (module S : MailboxStorage_intf with type loc = l and type param = p and type accs = a)
  params
  ?tp2
  ()
  =
  let (loc,m,i,param) = params in
  (module struct
    module MailboxStorage = S
    let this = S.create_st loc ~dirs:(m,i) param
    let this1 = match tp2 with | None -> None | Some (loc,m,i,param)-> 
      Some (S.create_st loc ~dirs:(m,i) param)
    end : Storage_inst with type MailboxStorage.accs = S.accs)
*)
let build_strg_inst
  (type l)
  (type p)
  (type a)
  (module S : MailboxStorage_intf with type loc = l and type param = p and type accs = a)
  params
  ?tp2
  ()
  =
  let (loc,m,i,param) = params in
  (module struct
    module MailboxStorage = S
    let this = S.create_st loc ~dirs:(m,i) param
    let this1 = match tp2 with | None -> None | Some (loc,m,i,param)-> 
      Some (S.create_st loc ~dirs:(m,i) param)
    end : Storage_inst)


(* define storage interface for index and mailboxes, for inst:
  mbox: all messages are contained in one file (mailbox)
  maildir: one file contains one message
  irminsule: the interface is through the API, not os primitives (primitives.ml
  implementation)
  Index drives mailbox navigation. The index contains uidvalidity and some
  statistics(header), uid, flags, and message location (message indices)
  Both index and mailbox have to be created, deleted, renamed, coppied
  (with filter), appended (with filter), updated, searched
  The index is fixed in that any record can be located via seq # or uid
  (either the same file, multiple files, or via API) 
  The mailbox is variable in that the message size is variable 
 *)

module MboxIndexAccessor 
  (P':Position) 
  (A':Accessor with type p = P'.t) :
  MboxIndexStorageAccessor_intf with type a = A'.t*int*int and type t = A'.t*int*int =
  struct
    type a = A'.t*int*int
    type t = a

    (* create accessor, descriptor, header size, record size *)
    let create desc = desc

    (* set position, return size of the record corresponding to the position *)
    let set_pos d pos hs rs =
      match pos with
      | `Position pos -> 
        if pos = 0 then (* header *)
          A'.seek_start d >>= fun _ -> return (`Ok hs)
        else (
          let pos = Int64.(+) 
            (Int64.of_int hs) 
            (Int64.( * ) (Int64.of_int (pos-1)) (Int64.of_int rs)) in
          A'.seek_set d (P'.of_int64 pos) >>= fun set_pos -> 
          if (P'.to_int64 set_pos) = pos then (
            return (`Ok rs)
          ) else (
            return `Eof
          )
        )
      | `Append -> A'.seek_end d >>= fun pos -> 
        let pos = P'.to_int64 pos in
        if pos = Int64.zero then
          return (`Ok hs)
        else
          return (`Ok rs)

    (* read block from the storage *)
    let reader tp pos =
      let (d,hs,rs) = tp in
      set_pos d pos hs rs >>= function
        | `Ok sz -> 
          (let blk = Block.from_int sz in
          A'.reader d blk >>= function
            | `Ok blk -> return (`Ok (Marshal.from_string (Block.content blk) 0))
            | `Eof i -> return `Eof
          )
        | `Eof -> return `Eof

    (* read index header *)
    let reader_header tp =
      reader tp (`Position 0) >>= function
        | `Eof -> assert(false)
        | `Ok hdr -> match hdr with
          | `Record _ -> assert(false)
          | `Header hdr -> return hdr

    (* read index record *)
    let reader_record tp pos =
      assert(pos <> (`Position 0));
      reader tp pos >>= function
        | `Eof -> return `Eof
        | `Ok hdr -> match hdr with
          | `Header _ -> assert(false)
          | `Record rc -> return (`Ok rc) (* this builds without `Ok!!! and crashes *)

    (* write block to the storage *)
    let writer tp pos blk = 
      let (d,hs,rs) = tp in
      set_pos d pos hs rs >>= function
        | `Ok sz ->
            let marshalled = Marshal.to_string blk [Marshal.Compat_32] in
            let marsh_len = String.length marshalled in
            if marsh_len > sz then
              assert(false)
            else
              let blk = Block.from_string (String.concat [marshalled;String.create (sz - marsh_len)]) in
             A'.writer d blk >>= fun _ -> return `Ok
        | `Eof -> return `Eof
        
    (* write index header *)
    let writer_header tp hdr =
      writer tp (`Position 0) (`Header hdr) >>= function
        | `Eof -> assert(false)
        | `Ok -> return ()
        
    (* write index record *)
    let writer_record tp pos rc =
      writer tp pos (`Record rc) >>= function
        | `Eof -> return `Eof
        | `Ok -> return `Ok

  end

module UnixMboxIndexStorageAccessor = MboxIndexAccessor 
  (BasicPosition) 
  (UnixAccessor)

module MboxMailboxAccessor 
    (P':Position) 
    (A':Accessor with type p = P'.t)
    (IDXAC':MboxIndexStorageAccessor_intf) : 
    MailboxAccessor_intf with type a = IDXAC'.t * A'.t and type t = IDXAC'.t * A'.t =
  struct
    type a = IDXAC'.t * A'.t
    type t = IDXAC'.t * A'.t

    (* create accessor *)
    let create a = a
    
    (* this a work around the template message that dovecot inserts as the
     * first message in a mailbox, this an inefficient implementation that
     * accesses the first record and if it is the template then it increments
     * the position by 1. An option is to have a flag in the index header and
     * read the header when the type is created. Another option is to have the
     * flag in the name of header or have a separate switch file.
     * should be changed TBD!!!
     *)
    let fudge_pos tp =
      let (ac,a) = tp in
      IDXAC'.reader_record ac (`Position 1) >>= function
        | `Ok rc ->
          if List.find rc.metadata.flags ~f:(fun fl -> fl = Flags_Template) <> None then
            return 1
          else
            return 0
        | _ -> return 0

    let upd_pos tp pos =
      fudge_pos tp >>= fun fudge ->
      match pos with | `Position pos -> return (`Position (pos + fudge))

    (* read block from the storage at requested position,
     * First read message offset from index and then read
     * the message; AC' is index accessor, A is mailbox accessor
     *)
    let reader tp ?filter pos = 
      let open Interpreter in
      let seq = match pos with `Position pos -> pos in
      let (ac,a) = tp in
      upd_pos tp pos >>= fun pos ->
      IDXAC'.reader_record ac pos >>= function
      | `Eof -> return `Eof
      | `Ok rc -> 
        let pos = P'.of_int64 rc.start_offset in
        A'.seek_set a pos >>= fun set ->
          if P'.compare pos set <> 0 then (
            return `Eof
          ) else
            let size = Int64.to_int_exn (Int64.(-) rc.end_offset rc.start_offset) in
            A'.reader a (Block.from_int size) >>= function
              | `Ok blk -> (* there must be only one message in the pipe *) 
                  Mailbox.With_pipe.of_string (Block.content blk) >>= fun mailbox ->
                    Pipe.fold mailbox (* is there a better way to get the message? TBD *)
                    ~init:[]
                    ~f:(fun acc message -> return (message::acc)) >>= fun messages -> 
                      let message = List.nth_exn messages 0 in
                      let metadata = rc.metadata in
                      if filter = None ||
                          exec_search message.email (Option.value_exn filter) metadata seq then
                        return (`Ok (message,metadata))
                      else
                        return `NotFound
              | `Eof _ -> return (`Eof)
    
    let find_flag l fl = 
      List.find l ~f:(fun f -> if f = fl then true else false) <> None

    (* read the metadata only *)
    let reader_metadata tp pos = 
      let (ac,_) = tp in
      IDXAC'.reader_record ac pos >>= function 
        | `Eof -> return `Eof
        | `Ok msg -> return (`Ok msg.metadata)

     (* Can only do append so Position is not applicable, nor is the header. 
      need to write back index header or accumulate header and have the
      fold handle cumulative header
     *)
    let writer tp pos blk = 
      let (ac,a) = tp in
      let (message,metadata) = blk in
      let block = (Block.from_string (Mailbox.Message.to_string message)) in
      (* read index header *)
      IDXAC'.reader_header ac >>= fun hdr -> 
      A'.seek_end a >>= fun pos ->
        let start_offset = P'.to_int64 pos in
        let metadata = {metadata with size = Block.size block;uid = hdr.uidnext} in
        let end_offset = Int64.(+) start_offset (Int64.of_int metadata.size) in
        let index = {metadata; start_offset; end_offset} in
        (* all writes have to be done as transaction TBD *)
        IDXAC'.writer_record ac `Append index >>= fun _ ->
          A'.writer a block >>= fun () -> 
            let count = hdr.count + 1 in
            let uidnext = hdr.uidnext + 1 in
            let seen = find_flag metadata.flags Flags_Seen in
            let nunseen = if seen = false then hdr.nunseen + 1 else hdr.nunseen in
            let recent =
              if find_flag metadata.flags Flags_Recent = true then hdr.recent + 1 else hdr.recent
            in
            let unseen =
              if hdr.unseen = 0 && seen = false then
                count
              else
                hdr.unseen
            in
            let hdr = update_mailbox_metadata ~header:hdr ~uidnext ~nunseen ~unseen ~recent ~count () in
            IDXAC'.writer_header ac hdr >>= fun () -> return `Ok

    (* write the metadata (flags only) *)
    let writer_metadata tp metadata pos =
      match pos with
      | `Position 0 -> assert(false)
      | `Position pos ->
      let (ac,_) = tp in
      (* read index header *)
      IDXAC'.reader_header ac >>= fun hdr -> 
        IDXAC'.reader_record ac (`Position pos) >>= function
          | `Eof -> return `Eof
          | `Ok msg ->
            (* only updating flags for now *)
            let metadata = { msg.metadata with flags = metadata.flags } in
            let msg = { msg with metadata } in
            IDXAC'.writer_record ac (`Position pos) msg >>= fun _ ->
            let seen = find_flag metadata.flags Flags_Seen in
            let nunseen = if seen = false then hdr.nunseen + 1 else hdr.nunseen in
            let recent =
              if find_flag metadata.flags Flags_Recent = true then hdr.recent + 1 else hdr.recent
            in
            let unseen =
              if hdr.unseen = 0 && seen = false && pos < hdr.unseen then
                pos
              else
                hdr.unseen
            in
            let hdr = update_mailbox_metadata ~header:hdr ~nunseen ~unseen ~recent () in
            IDXAC'.writer_header ac hdr >>= fun () -> return `Ok
  end

module UnixMboxMailboxStorageAccessor = MboxMailboxAccessor
  (BasicPosition)
  (UnixAccessor) 
  (UnixMboxIndexStorageAccessor)

module MboxLocationConstructor (L':Location) : 
  LocationConstructor_intf with type t = L'.t =
  struct
    type t = L'.t

    let to_string t = L'.to_str t

    let construct t ~dirs st_type =
      match st_type with
      | `Mailbox -> t
      | `Index -> 
        let dirname = L'.dirname t in
        let (mailbox_root, inbox_root) = dirs in
        let path =
          if (L'.to_str inbox_root) = dirname then
            (L'.to_str mailbox_root) ^ "/.imaplet/INBOX"
          else
            dirname ^ "/.imaplet/" ^ (L'.basename t) 
        in
        let loc = L'.create path in
        loc
  end

module MboxBasicLocationConstructor = MboxLocationConstructor
  (BasicLocation)

module MboxIndexStorage 
    (P':Permissions) 
    (F':Flags) 
    (L':Location) 
    (LC':LocationConstructor_intf with type t = L'.t) 
    (AC':Accessor)
    (U':Utils with type fl = F'.t and type loc = L'.t and type perms = P'.t and type accs = AC'.t)
    (IDXAC':MboxIndexStorageAccessor_intf with type a = AC'.t*int*int) : 
    MboxIndexStorage_intf with type loc = L'.t and type param = int*int and type t = L'.t*int*int and 
    type accs = IDXAC'.t =
  struct
    type loc = L'.t
    type param = int * int
    type t = L'.t*int*int
    type accs = IDXAC'.t

    (* create storage type; takes location of the mailbox as a client sees it;
     * creates specific implementation location
     *)
    let create_st lc ~dirs param =
      let (hs,rs) = param in
      ((LC'.construct lc ~dirs `Index),hs,rs)

    let get_loc tp =
      let (l,_,_) = tp in l

    let fold_internal tp ?(exclusive=false) ~perm ~flags ~init ~f =
      let (l,hs,rs) = tp in
      U'.fold l ~exclusive ~perm ~flags ~init ~f:(fun acc accs ->
        let accs = IDXAC'.create (accs,hs,rs) in
        f acc accs
      )

    (* check if storage exists *)
    let exists tp = U'.exists (get_loc tp)

    (* create storage *)
    let create tp = 
      let perm = P'.create 0o666 in
      U'.create ~perm (get_loc tp) >>= function
        | `Folder -> return `Folder
        | `Storage ->
          let flags = F'.create [Nonblock;Wronly] in
          fold_internal tp ~exclusive:true ~perm ~flags ~init:() ~f:(fun () accs ->
            let hdr = empty_mailbox_metadata ~uidvalidity:(new_uidvalidity()) () in
            IDXAC'.writer accs (`Position 0) (`Header hdr) >>= function
              | `Eof -> assert(false)
              | `Ok -> return ()
          ) >>= fun () -> return `Storage

    (* move storage *)
    let move tp1 tp2 = 
      U'.move (get_loc tp1) (get_loc tp2)

    (* delete storage *)
    let delete tp = U'.delete (get_loc tp)

    let fold tp ?(exclusive=false) ~init ~f =
      let perm = P'.create 0o666 in
      let flags = F'.create [Nonblock;Rdwr] in
      fold_internal tp ~exclusive ~perm ~flags ~init ~f

    (* list storage only as the folders are not applicable in this case *)
    let list_store tp ~init ~f = 
      let (start,_,_) = tp in
      let suffix = L'.create "" in
      U'.list_store ~start ~suffix ~exclude:"" ~init ~f:(fun acc item ->
        match item with
        | `Folder _ -> return acc
        | `Storage item ->
          let str = L'.to_str item in
          if match_regex str "^\\.imaplet" then
            f acc (`Storage str)
          else
           return acc
      )

    let rec docopy accs1 accs2 f cnt =
      let pos = `Position cnt in
      IDXAC'.reader accs1 pos >>= function
        | `Eof -> return ()
        | `Ok blk -> 
          if f cnt blk = true then
            IDXAC'.writer accs2 pos blk >>= function
              | `Eof -> return ()
              | `Ok -> docopy accs1 accs2 f (cnt+1)
          else
            docopy accs1 accs2 f (cnt+1)
          
    (* copy storage with filter *)
    let copy tp1 tp2 ~f = 
      exists tp1 >>= function
        | `No | `Folder -> return `SrcNotExists
        | `Storage -> exists tp2 >>= function
          | `Folder | `No -> return `DestNotExists
          | `Storage -> create tp2 >>= fun _ ->
            fold tp1 ~exclusive:false ~init:() ~f:(fun () accs1 ->
              fold tp2 ~exclusive:false ~init:() ~f:(fun () accs2 ->
                docopy accs1 accs2 f 1
              )
            ) >>= fun () -> return `Ok

  end

module UnixMboxIndexStorage = MboxIndexStorage 
  (BasicPermissions) 
  (UnixFlags) 
  (BasicLocation) 
  (MboxBasicLocationConstructor) 
  (UnixAccessor) 
  (UnixUtils) 
  (UnixMboxIndexStorageAccessor) 

module MboxMailboxStorage 
  (POS':Position)
  (P':Permissions) 
  (F':Flags)
  (L':Location) 
  (LC':LocationConstructor_intf with type t = L'.t)
  (AC':Accessor with type p = POS'.t)
  (U':Utils with type loc = L'.t and type perms=P'.t and type fl = F'.t and type accs=AC'.t)
  (IDXAC':MboxIndexStorageAccessor_intf)
  (MBXAC':MailboxAccessor_intf with type a = IDXAC'.t*AC'.t and type t = IDXAC'.t*AC'.t)
  (IDXS':MboxIndexStorage_intf with type loc = L'.t and type param = int*int and type t = L'.t*int*int and 
    type accs = IDXAC'.t) :
  MailboxStorage_intf with type loc = L'.t and type param = int * int and type t =
    L'.t*L'.t*L'.t*int*int and type accs = MBXAC'.t =
  struct
    type loc = L'.t
    type param = int * int
    type t = L'.t * L'.t * L'.t * int * int
    type accs = MBXAC'.t

    let accessor_factory a =
      build_accs_inst (module MBXAC')  a

    (* create storage type; takes location of the mailbox as a client sees it;
     * creates specific implementation location
     *)
    let create_st lc ~dirs param =
      let (mbox,index) = dirs in
      let (hs,rs) = param in
      ((LC'.construct lc ~dirs `Mailbox),mbox,index,hs,rs)

    let get_loc tp =
      let (l,_,_,_,_) = tp in l

    let create_idx_str_inst tp =
      let (l,m,i,hs,rs) = tp in
      IDXS'.create_st l ~dirs:(m,i) (hs,rs)

    let create_idx_storage tp =
      let st = create_idx_str_inst tp in
      IDXS'.create st

    (* check if storage exists 
     * assume that index exists 
     *)
    let exists tp = U'.exists (get_loc tp)

    (* create storage *)
    let create tp = 
      (* create index *)
      create_idx_storage tp >>= function
        | `Folder -> return `Folder
        | `Storage -> U'.create (get_loc tp)

    (* move storage *)
    let move tp1 tp2 = 
      let tp1' = create_idx_str_inst tp1 in
      let tp2' = create_idx_str_inst tp2 in
      IDXS'.move tp1' tp2' >>= fun () ->
        U'.move (get_loc tp1) (get_loc tp2)

    (* delete storage *)
    let delete tp = 
      let idx = create_idx_str_inst tp in
      (* delete index *)
      IDXS'.delete idx >>= fun() ->
        U'.delete (get_loc tp)

    (* update index from the mailbox *)
    let update_index tp = 
      let idx = create_idx_str_inst tp in
      IDXS'.exists idx >>= function
        | `No | `Folder -> return `NotExists
        | `Storage ->
      (* fold over structured index *)
      IDXS'.fold ~exclusive:true idx 
      ~init:() 
      ~f:(fun () idx_accs -> 
        IDXAC'.reader_header idx_accs >>= fun hdr ->
          begin
          if hdr.count = 0 then
            return Int64.zero
          else
            (* read the last record in the index *)
            IDXAC'.reader_record idx_accs (`Position hdr.count) >>= function
              | `Eof -> assert(false)
              | `Ok rc -> return rc.end_offset
          end >>= fun new_msg_offset -> (* offset of a new message into the raw
          mailbox *)
          let perm = P'.create 0o666 in
          let flags = F'.create [Nonblock;Rdwr] in
          (* fold over the raw mailbox *)
          U'.fold ~exclusive:true (get_loc tp) ~perm ~flags 
          ~init:() 
          ~f:(fun () mbx_accs -> 
            (* set position to the last message according to the index *)
            AC'.seek_set mbx_accs (POS'.of_int64 new_msg_offset) >>= fun pos ->
              if (POS'.to_int64 pos) <> new_msg_offset then
                assert(false)
              else
                let pipe_read = Pipe.init (fun w -> 
                  (* read raw bytes from the mailbox and pass to Email_message
                   * for parsing
                   *)
                  let rec rd_mbox () =
                    if Pipe.is_closed w then
                      return ()
                    else
                      let blk = Block.from_int 1024 in
                      AC'.reader mbx_accs blk >>= function
                        | `Ok blk -> Pipe.write w (Block.content blk) >>= fun () -> rd_mbox()
                        | `Eof blk -> 
                          if (Block.size blk) = 0 then (
                            Pipe.close w; return ()
                          ) else (
                            Pipe.write w (Block.content blk) >>= fun () ->
                              Pipe.close w; return ()
                          )
                  in
                  rd_mbox()
                ) in
                Reader.of_pipe (Info.of_string "controlled network reader") pipe_read >>= fun c_reader ->
                Mailbox.With_pipe.t_of_fd (Reader.fd c_reader) >>= fun mailbox ->
                (* fold over structured email messages *)
                Pipe.fold mailbox 
                ~init:(hdr,new_msg_offset) 
                ~f:(fun (hdr,offset) message -> 
                  let from_daemon = (message.postmark.from = "MAILER_DAEMON") in
                  let idx_record = empty_mbox_message_metadata() in
                  let start_offset = offset in
                  (* screwy but email_message after parsing doesn't have a space
                   * between the address and date part in the postmark
                   *)
                  let size = String.length (Mailbox.Message.to_string message) + 1 in
                  let end_offset = Int64.(+) start_offset (Int64.of_int size) in
                  let internal_date = Time.now() in
                  let uid = if from_daemon then hdr.uidnext else hdr.uidnext + 1 in
                  let flags = if from_daemon then [Flags_Template] else [] in
                  let idx_record = update_mbox_message_metadata ~data:idx_record
                  ~start_offset ~end_offset ~size ~internal_date ~uid ~flags () in
                  IDXAC'.writer_record idx_accs `Append idx_record >>= fun _ ->
                   if from_daemon then
                     return (hdr,end_offset)
                   else (
                     let nunseen = hdr.nunseen + 1 in
                     let unseen =
                      if hdr.unseen = 0 then
                        hdr.count
                      else
                        hdr.unseen
                     in
                     let recent = hdr.recent + 1 in
                     let count = hdr.count + 1 in
                     let uidnext = hdr.uidnext + 1 in
                     let hdr = update_mailbox_metadata ~header:hdr ~nunseen ~unseen
                     ~recent ~count ~uidnext ()
                     in
                     IDXAC'.writer_header idx_accs hdr >>= fun () -> return (hdr,end_offset) 
                   )
                ) >>= fun _ -> return ()
          )
      ) >>= fun () -> return `Ok

    (* rebuild entier index for the mailbox *)
    let rebuild_index tp = 
      let idx = create_idx_str_inst tp in
      IDXS'.delete idx >>= fun () ->
        IDXS'.create idx >>= function 
          | `Folder -> assert(false)
          | `Storage -> update_index tp >>= fun _ -> return ()

    (* build index if it doesn't exist *)
    let build_index_if_not_exists tp =
      let idx = create_idx_str_inst tp in
      IDXS'.exists idx >>= function
        | `No -> rebuild_index tp
        | _ -> return ()

    (* fold over index then over mailbox *)
    let fold tp ?(exclusive=false) ~init ~f = 
      let idx = create_idx_str_inst tp in
      build_index_if_not_exists tp >>= fun () ->
      (* index access *)
      IDXS'.fold idx ~exclusive ~init ~f:(fun acc idx_accs ->
        IDXAC'.reader_header idx_accs >>= fun h -> (* do I need the header?  REVIEW TBD *)
          let l = get_loc tp in
          let perm = P'.create 0o666 in
          let flags = F'.create [Nonblock;Rdwr] in
          U'.fold l ~exclusive ~perm ~flags ~init:acc ~f:(fun acc accs ->
            (*let mbx_accs = MBXAC'.create (idx_accs, accs) in *)
            let mbx_accs = accessor_factory (idx_accs, accs) in
            f acc mbx_accs
          )
        )

    (* list storage *)
    let list_store tp ~init ~f = 
      let (_,m,_,_,_) = tp in
      let to_str item = L'.to_str item in
      let start = get_loc tp in
      let suffix = L'.create "" in
      let subscr = ((to_str m) ^ "/.subscriptions") in
      let exclude = "^\\(.*/\\)?\\(\\(\\.imaplet\\)\\|\\(\\.imap\\)\\)\\(/.*\\)?$" in
      U'.list_store ~start ~suffix ~exclude ~init ~f:(fun acc item ->
        match item with
        | `Folder (item,cnt) -> f acc (`Folder ((to_str item),cnt))
        | `Storage item -> 
          if to_str item = subscr then 
            return acc
          else
            f acc (`Storage (to_str item))
      )

    let rec docopy accs1 accs2 f cnt =
      let pos = `Position cnt in
      let (module Accessor:StorageAccessor_inst) = accs1 in
      Accessor.StorageAccessor.reader Accessor.this pos >>= function
        | `Eof -> return ()
        | `NotFound -> docopy accs1 accs2 f (cnt + 1)
        | `Ok blk -> 
          if f cnt blk = true then
            let (module Accessor:StorageAccessor_inst) = accs2 in
            Accessor.StorageAccessor.writer Accessor.this `Append blk >>= function
              | `Ok -> docopy accs1 accs2 f (cnt+1)
          else
            docopy accs1 accs2 f (cnt+1)

    (* copy storage with filter *)
    let copy tp1 tp2 ~f = 
      exists tp1 >>= function
        | `No | `Folder -> return `SrcNotExists
        | `Storage -> exists tp2 >>= function
          | `Folder | `No -> return `DestNotExists
          | `Storage -> create tp2 >>= fun _ ->
              fold tp1 ~exclusive:false ~init:() ~f:(fun () accs1 ->
                fold tp2 ~exclusive:false ~init:() ~f:(fun () accs2 ->
                docopy accs1 accs2 f 1
              )
            ) >>= fun () -> return `Ok

    let copy_with tp1 tp2 ~filter = 
      exists tp1 >>= function
        | `No | `Folder -> return `SrcNotExists
        | `Storage -> exists tp2 >>= function
          | `Folder | `No -> return `DestNotExists
          | `Storage -> create tp2 >>= fun _ -> 
            let (buid,sequence) = filter in
            copy tp1 tp2 ~f:(fun seq (_,metadata) ->
              let id = (if buid then metadata.uid else seq) in
              (Interpreter.exec_seq sequence id)
            )

    let rec docopy accs1 accs2 f cnt =
      let pos = `Position cnt in
      let (module Accessor:StorageAccessor_inst) = accs1 in
      Accessor.StorageAccessor.reader Accessor.this pos >>= function
        | `Eof -> return ()
        | `NotFound -> docopy accs1 accs2 f (cnt+1)
        | `Ok blk -> 
          if f cnt blk = true then
            let (module Accessor:StorageAccessor_inst) = accs2 in
            Accessor.StorageAccessor.writer Accessor.this `Append blk >>= function
              | `Ok -> docopy accs1 accs2 f (cnt+1)
          else
            docopy accs1 accs2 f (cnt+1)

    (* search with filter *)
    let search_with t ~filter =
      let (buid,keys) = filter in
      let rec doread acc accs seq =
        let (module Accessor : StorageAccessor_inst) = accs in
        Accessor.StorageAccessor.reader Accessor.this (`Position seq) >>= function
          | `Eof -> return acc
          | `NotFound -> doread acc accs (seq + 1)
          | `Ok (message,metadata) -> 
            let res = Interpreter.exec_search message.email keys metadata seq in 
            if res then
              let selected = if buid then metadata.uid else seq in
              doread (selected :: acc) accs (seq + 1)
            else
              doread acc accs (seq + 1)
        in
      fold t ~exclusive:false ~init:[] ~f:(fun acc accs ->
        doread acc accs 1
      )

    (* expunge messages with \Deleted flag 
     * need to review, copy/move will have a
     * new uidvalidity and uid TBD!!! 
     *)
    let expunge tp1 ?tmp:tp2 ~f =
      assert(tp2 = None);
      let tp2 = Option.value_exn tp2 in
      copy tp1 tp2 ~f:(fun seq (_,metadata) ->
        let retain =
        List.find metadata.flags 
          ~f:(fun fl -> if fl = Flags_Deleted then true else false) = None 
        in
        if retain then
          true
        else (
          f seq;
          false
        )
      ) >>= fun _ -> move tp2 tp1 

    (* get mailbox metadata like uidvalidity and some stats *)
    let get_mailbox_metadata tp =
      let idx = create_idx_str_inst tp in
      build_index_if_not_exists tp >>= fun() ->
        IDXS'.fold ~exclusive:true idx 
        ~init:(empty_mailbox_metadata())
        ~f:(fun acc idx_accs -> 
          IDXAC'.reader_header idx_accs 
        )

    (** get subscription path **)
    let get_subscr_path m = 
      let loc = L'.concat m (L'.create ".subscriptions") in
      L'.create loc

    (* subscription helper *)
    let subscr_helper tp ~flags ~init ~f =
      let (_,m,_,_,_) = tp in
      let path = get_subscr_path m in
      let u = U'.create path in
      let perm = P'.create 0o666 in
      let flags = F'.create flags in
      U'.fold path ~exclusive:true ~perm ~flags ~init:init ~f:(fun acc accs ->
        f acc accs
      )

    (* get list of subscribed mailboxes *)
    let get_subscription tp =
      let (_,m,_,_,_) = tp in
      subscr_helper tp ~flags:[Nonblock;Rdonly] ~init:[] ~f:(fun acc accs ->
        let rec read acc accs =
          AC'.reader_line accs >>= function
            | `Ok blk -> read ((Block.content blk) :: acc) accs
            | `Eof -> return acc
        in
        read acc accs
      )

    (* subscribe the mailbox *)
    let subscribe tp = 
      let (_,m,_,_,_) = tp in
      subscr_helper tp ~flags:[Nonblock;Rdwr] ~init:() ~f:(fun () accs ->
        let rec read accs =
          AC'.reader_line accs >>= function
            | `Ok blk -> 
             if (Block.content blk) = (L'.to_str m) then
               return ()
             else
               read accs
            | `Eof ->
               AC'.writer_line accs (Block.from_string (L'.to_str m)) 
        in
        read accs
      )

    (* unsubscribe the mailbox *)
    let unsubscribe tp = 
      let (_,m,_,_,_) = tp in
      subscr_helper tp ~flags:[Nonblock;Rdonly] ~init:([],false) ~f:(fun acc accs ->
        let rec read (acc,b) accs =
          AC'.reader_line accs >>= function
            | `Ok blk -> 
             if (Block.content blk) = (L'.to_str m) then
               read (acc,true) accs
             else
               read (((Block.content blk) :: acc),b) accs
            | `Eof -> return (acc,b)
        in
        read acc accs
      ) >>= fun (l,b) ->
        if b = false then
          return ()
        else
          subscr_helper tp ~flags:[Nonblock;Trunc;Wronly] ~init:() ~f:(fun () accs ->
            Deferred.List.iter l ~f:(fun mbox -> AC'.writer_line accs (Block.from_string mbox))
          ) 

    let create_account tp = return `Ok (* TBD *)

    let remove_account tp = return `Ok (* TBD *)
    

  end

module UnixMboxMailboxStorage = MboxMailboxStorage 
  (BasicPosition)
  (BasicPermissions)
  (UnixFlags)
  (BasicLocation)
  (MboxBasicLocationConstructor) 
  (UnixAccessor)
  (UnixUtils) 
  (UnixMboxIndexStorageAccessor)
  (UnixMboxMailboxStorageAccessor)
  (UnixMboxIndexStorage)
