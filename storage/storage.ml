open Core.Std
open Async.Std
open Primitives
open Email_message
open Mflags

type mailbox_metadata = {
  uidvalidity: string;
  uidnext: int;
  modseq: int64;
  count: int;
  unseen: int;
  nunseen: int;
  recent: int;
}

type mailbox_message_metadata = {
  uid: int;
  modseq: int64;
  internal_date: Time.t;
  size: int;
  flags: mailboxFlags list;
}

type mbox_message_metadata = {
  metadata: mailbox_message_metadata;
  start_offset: int64;
  end_offset: int64;
}

type mbox_index = [`Header of mailbox_metadata | `Record of mbox_message_metadata]

let new_uidvalidity() =
  let t = Time.now() in
  let f = (Time.to_float t) in
  string_of_int (Float.to_int f)

let empty_mailbox_metadata ?uidvalidity () =
  let uidvalidity =
    (match uidvalidity with None->""|Some u -> u) in
  {uidvalidity;modseq=Int64.zero;uidnext=0;count=0;unseen=0;nunseen=0;recent=0}

let empty_mailbox_message_metadata() =
  {uid=Int.neg 1;modseq=Int64.zero;internal_date=Time.now();size=0;flags=[]}

let empty_mbox_message_metadata() =
  {metadata=empty_mailbox_message_metadata();start_offset=Int64.zero;end_offset=Int64.zero}

let update_mailbox_metadata ~header ?uidvalidity ?uidnext ?modseq
?count ?unseen ?nunseen ?recent () : (mailbox_metadata) =
  let uidvalidity=(match uidvalidity with None->header.uidvalidity|Some u->u) in
  let uidnext=(match uidnext with None->header.uidnext|Some u->u) in
  let modseq=(match modseq with None->header.modseq|Some m->m) in
  let count=(match count with None->header.count|Some c->c) in
  let unseen=(match unseen with None->header.unseen|Some u->u) in
  let nunseen=(match nunseen with None->header.nunseen|Some n->n) in
  let recent=(match recent with None->header.recent|Some r->r) in
  {uidvalidity;modseq;uidnext;count;unseen;nunseen;recent}

let update_mailbox_message_metadata ~data ?uid ?modseq
?internal_date ?size ?flags () : (mailbox_message_metadata) =
  let uid =
    (match uid with |None -> data.uid|Some uid -> uid) in
  let modseq =
    (match modseq with |None -> data.modseq|Some modseq -> modseq) in
  let internal_date =
    (match internal_date with |None->data.internal_date|Some d -> d) in
  let size =
    (match size with |None->data.size|Some i -> i) in
  let flags = 
    (match flags with |None->data.flags|Some f->f) in
  {uid;modseq;internal_date;size;flags}

let update_mbox_message_metadata ~data ?uid ?modseq
?internal_date ?size ?start_offset ?end_offset ?flags () : (mbox_message_metadata) =
  let metadata = update_mailbox_message_metadata ~data:data.metadata ?uid ?modseq ?internal_date
  ?size ?flags () in
  let start_offset =
    (match start_offset with |None->data.start_offset|Some i -> i) in
  let end_offset =
    (match end_offset with |None->data.end_offset|Some i -> i) in
  {metadata;start_offset;end_offset}

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
      [`Ok of blk|`Eof|`OutOfBounds] Deferred.t

    (* write block to the storage at requested position, 
     * If `OutOfBounds then requested position exceeds storage size
     * If `InvalidBlock then the block size is invalid `Header of hdr |`Record
     * of rec * int
     *)
    val writer : t -> [`Append|`Position of int] -> blk ->
      [`Ok|`OutOfBounds|`InvalidBlock] Deferred.t
  end

module type MboxIndexStorageAccessor_intf =
  sig
    include StorageAccessor_intf with type blk := mbox_index

    val reader_header : t -> mailbox_metadata Deferred.t

    val reader_record : t -> [`Position of int] -> 
      [`Ok of mbox_message_metadata|`Eof|`OutOfBounds] Deferred.t

    val writer_header : t -> mailbox_metadata -> unit Deferred.t

    val writer_record : t -> [`Append|`Position of int] -> mbox_message_metadata ->
      [`Ok|`OutOfBounds] Deferred.t
  end

type mailbox_data = Mailbox.Message.t * mailbox_message_metadata

module type MailboxAccessor_intf = 
  sig
    include StorageAccessor_intf with type blk := mailbox_data

    val reader_metadata : t -> [`Position of int] ->
      [`Ok of mailbox_message_metadata|`Eof|`OutOfBounds] Deferred.t

    val writer_metadata : t -> mailbox_message_metadata -> [`Position of int] ->
      [`Ok |`OutOfBounds] Deferred.t
  end

module type StorageAccessor_inst = 
  sig
    module StorageAccessor : MailboxAccessor_intf
    val this : StorageAccessor.t
  end

let build_accs_inst
  (type a)
  (module SA : MailboxAccessor_intf with type a = a)
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
      f:('a -> [`Folder of string*int|`Storage of string] -> 'a) -> 'a Deferred.t

    (* copy storage with filter *)
    val copy : t -> t -> f:(int -> blk -> bool) -> [`Ok|`SrcNotExists|`DestExists] Deferred.t

  end

module type MboxIndexStorage_intf = Storage_intf with type blk := mbox_index

module type MailboxStorage_intf = 
  sig
    include Storage_intf with 
      type blk := mailbox_data

    (* read/update storage, overwriting *)
    val fold : t -> ?exclusive:bool -> init:'a -> f:('a -> (module StorageAccessor_inst) -> 'a Deferred.t) -> 'a Deferred.t

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
  end


module type Storage_inst = 
  sig
    module MailboxStorage : MailboxStorage_intf
    val this : MailboxStorage.t
    val this1 : MailboxStorage.t option
  end

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
      | `Position pos -> printf "setting position %d%!" pos;
        if pos = 0 then (* header *)
          A'.seek_start d >>= fun _ -> return (`Ok hs)
        else (
          let pos = Int64.(+) 
            (Int64.of_int hs) 
            (Int64.( * ) (Int64.of_int (pos-1)) (Int64.of_int rs)) in
          printf " offset %s%!" (Int64.to_string pos);
          A'.seek_set d (P'.of_int64 pos) >>= fun set_pos -> 
          if (P'.to_int64 set_pos) = pos then (
            printf " ok\n%!";
            return (`Ok rs)
          ) else (
            printf " outofbounds\n%!";
            return `OutOfBounds
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
            | `Eof i -> printf "got index eof %d\n%!" (Block.size i); return `Eof
          )
        | `OutOfBounds -> return `OutOfBounds

    (* read index header *)
    let reader_header tp =
      reader tp (`Position 0) >>= function
        | `Eof | `OutOfBounds -> assert(false)
        | `Ok hdr -> match hdr with
          | `Record _ -> assert(false)
          | `Header hdr -> return hdr

    (* read index record *)
    let reader_record tp pos =
      assert(pos <> (`Position 0));
      reader tp pos >>= function
        | `Eof -> return `Eof
        | `OutOfBounds -> return `OutOfBounds
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
              return `InvalidBlock
            else
              let blk = Block.from_string (String.concat [marshalled;String.create (sz - marsh_len)]) in
             A'.writer d blk >>= fun _ -> return `Ok
        | `OutOfBounds -> return `OutOfBounds
        
    (* write index header *)
    let writer_header tp hdr =
      writer tp (`Position 0) (`Header hdr) >>= function
        | `OutOfBounds | `InvalidBlock -> assert(false)
        | `Ok -> return ()
        
    (* write index record *)
    let writer_record tp pos rc =
      writer tp pos (`Record rc) >>= function
        | `InvalidBlock -> assert(false)
        | `OutOfBounds -> return `OutOfBounds
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
    let reader tp pos = 
      let (ac,a) = tp in
      upd_pos tp pos >>= fun pos ->
      IDXAC'.reader_record ac pos >>= function
      | `Eof -> printf "got eof while reading index\n%!"; return `Eof
      | `OutOfBounds -> return `OutOfBounds
      | `Ok rc -> 
        printf "offset %d start %s end %s\n%!" (match pos with `Position pos ->
          pos) (Int64.to_string rc.start_offset)
        (Int64.to_string rc.end_offset);
        let pos = P'.of_int64 rc.start_offset in
        A'.seek_set a pos >>= fun set ->
          if P'.compare pos set <> 0 then (
            printf "got eof %s %s\n%!" (P'.to_string pos) (P'.to_string pos);
            return `Eof
          ) else
            let size = Int64.to_int_exn (Int64.(-) rc.end_offset rc.start_offset) in
            printf "reading size %d\n%!" size;
            A'.reader a (Block.from_int size) >>= function
              | `Ok blk -> (* there must be only one message in the pipe *) 
                  Mailbox.With_pipe.of_string (Block.content blk) >>= fun mailbox ->
                    Pipe.fold mailbox (* is there a better way to get the message? TBD *)
                    ~init:[]
                    ~f:(fun acc message -> return (message::acc)) >>= fun messages -> 
                      return (`Ok (List.nth_exn messages 0,rc.metadata)) (* should check
                      the length TBD *)
              | `Eof _ -> printf "got eof\n%!"; return (`Eof)
    
    let find_flag l fl = 
      List.find l ~f:(fun f -> if f = fl then true else false) <> None

    (* read the metadata only *)
    let reader_metadata tp pos = 
      let (ac,_) = tp in
      IDXAC'.reader_record ac pos >>= function 
        | `Eof -> return `Eof
        | `OutOfBounds -> return `OutOfBounds
        | `Ok msg -> return (`Ok msg.metadata)

     (* Can only do append so Position is not applicable, nor is the 
     * header. Consequently OutOfBounds|InvalidBlock is n/a TBD
      need to write back index header or accumulate header and have the
      fold handle cumulative header
     *)
    let writer tp pos blk = 
      assert(pos = `Append);
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
          | `Eof -> return `OutOfBounds
          | `OutOfBounds -> return `OutOfBounds
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
              | `InvalidBlock | `OutOfBounds -> assert(false)
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
        | `Folder _ -> acc
        | `Storage item ->
          let str = L'.to_str item in
          if match_regex str "^\\.imaplet" then
            f acc (`Storage str)
          else
           acc
      )

    let rec docopy accs1 accs2 f cnt =
      let pos = `Position cnt in
      IDXAC'.reader accs1 pos >>= function
        | `Eof | `OutOfBounds -> return ()
        | `Ok blk -> 
          if f cnt blk = true then
            IDXAC'.writer accs2 pos blk >>= function
              | `OutOfBounds | `InvalidBlock -> return ()
              | `Ok -> docopy accs1 accs2 f (cnt+1)
          else
            docopy accs1 accs2 f (cnt+1)
          
    (* copy storage with filter *)
    let copy tp1 tp2 ~f = 
      exists tp1 >>= function
        | `No | `Folder -> return `SrcNotExists
        | `Storage -> exists tp2 >>= function
          | `Folder | `Storage -> return `DestExists
          | `No -> create tp2 >>= fun _ ->
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
              | `Eof | `OutOfBounds -> assert(false)
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
                let r = Pipe.init (fun w -> 
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
                Mailbox.With_pipe.t_of_pipe r >>= fun mailbox ->
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
                  IDXAC'.writer_record idx_accs `Append idx_record >>= function
                    | `OutOfBounds -> assert(false)
                    | `Ok ->
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
            acc
          else
            f acc (`Storage (to_str item))
      )

    let rec docopy accs1 accs2 f cnt =
      let pos = `Position cnt in
      let (module Accessor:StorageAccessor_inst) = accs1 in
      Accessor.StorageAccessor.reader Accessor.this pos >>= function
        | `Eof | `OutOfBounds -> return ()
        | `Ok blk -> 
          if f cnt blk = true then
            let (module Accessor:StorageAccessor_inst) = accs2 in
            Accessor.StorageAccessor.writer Accessor.this pos blk >>= function
              | `OutOfBounds | `InvalidBlock -> return ()
              | `Ok -> docopy accs1 accs2 f (cnt+1)
          else
            docopy accs1 accs2 f (cnt+1)

    (* copy storage with filter *)
    let copy tp1 tp2 ~f = 
      exists tp1 >>= function
        | `No | `Folder -> return `SrcNotExists
        | `Storage -> exists tp2 >>= function
          | `Folder | `Storage -> return `DestExists
          | `No -> create tp2 >>= fun _ ->
              fold tp1 ~exclusive:false ~init:() ~f:(fun () accs1 ->
                fold tp2 ~exclusive:false ~init:() ~f:(fun () accs2 ->
                docopy accs1 accs2 f 1
              )
            ) >>= fun () -> return `Ok

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
