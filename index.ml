open Core.Std
open Async.Std
open Async_unix

exception LongHeader
exception LongRecord
exception FailedOffset
exception FailedHeaderWrite
exception FailedEof
exception NormalEof
exception InvalidIndex

let block_size = 1024

type mbox_header = {
  uidvalidity: string;
  uidnext: int;
  count: int;
  unseen: int;
  nunseen: int;
  recent: int;
}

type mbox_msg_metadata = {
  uid: int;
  internal_date: Time.t;
  size: int; (** message size **)
  start_offset: int64;
  end_offset: int64;
  flags: string list; (** marshal doesn't work with map? TBD **)
}

type skipIndex = Header|Records of int

type writeRecordCb = (mbox_msg_metadata -> unit Deferred.t)
type mboxHeaderCb = (unit -> mbox_header)

let print_header header =
	printf  "v:%s u:%d c:%d u:%d n:%d r:%d\n%!" header.uidvalidity header.uidnext header.count header.unseen 
		header.nunseen header.recent
		
let print_metadata metadata =
	printf "u:%d d:%s %s %s %s %d\n%!" metadata.uid (Time.to_string metadata.internal_date) 
	(String.concat ~sep:" " metadata.flags) (Int64.to_string metadata.start_offset)
        (Int64.to_string metadata.end_offset) metadata.size

let new_uidvalidity() =
  let t = Time.now() in
  let f = (Time.to_float t) in
  string_of_int (Float.to_int f)

let empty_mbox_header ?uidvalidity () =
  let uidvalidity =
    (match uidvalidity with None->""|Some u -> u) in
  {uidvalidity;uidnext=0;count=0;unseen=0;nunseen=0;recent=0}

let empty_mbox_msg_metadata() =
  {uid=Int.neg 1;internal_date=Time.now();size=0;start_offset=Int64.zero;end_offset=Int64.zero;flags=[]}

let update_mbox_header ~header ?uidvalidity ?uidnext
?count ?unseen ?nunseen ?recent () : (mbox_header) =
  let uidvalidity=(match uidvalidity with None->header.uidvalidity|Some u->u) in
  let uidnext=(match uidnext with None->header.uidnext|Some u->u) in
  let count=(match count with None->header.count|Some c->c) in
  let unseen=(match unseen with None->header.unseen|Some u->u) in
  let nunseen=(match nunseen with None->header.nunseen|Some n->n) in
  let recent=(match recent with None->header.recent|Some r->r) in
  {uidvalidity;uidnext;count;unseen;nunseen;recent}

let update_msg_metadata ~data ?uid
?internal_date ?size ?start_offset ?end_offset ?flags () : (mbox_msg_metadata) =
  let uid =
    (match uid with |None -> data.uid|Some uid -> uid) in
  let internal_date =
    (match internal_date with |None->data.internal_date|Some d -> d) in
  let size =
    (match size with |None->data.size|Some i -> i) in
  let start_offset =
    (match start_offset with |None->data.start_offset|Some i -> i) in
  let end_offset =
    (match end_offset with |None->data.end_offset|Some i -> i) in
  let flags = 
    (match flags with |None->data.flags|Some f->f) in
  {uid;internal_date;size;start_offset;end_offset;flags}

(** set the offset to the record boundary **)
let set_offset_exn (fd:Fd.t) ?(mode=`Set) (offset:int64) : (unit Deferred.t) =
  if mode = `Cur then
    raise FailedOffset
  else (
    Unix_syscalls.lseek fd offset ~mode >>= fun pos ->
    if mode = `Set && Int64.compare offset pos <> 0 then
      raise FailedOffset
    else
     return ()
  )

(** rewind one record - should be only done after the first record is read **)
let rewind_record (fd:Fd.t) : (unit Deferred.t) =
  let offset = Int64.neg (Int64.of_int block_size) in
  Unix_syscalls.lseek fd offset ~mode:`Cur >>= fun pos -> return()

(** get current offset **)
let cur_offset (fd:Fd.t) : (int64 Deferred.t) =
  Unix_syscalls.lseek fd Int64.zero ~mode:`Cur

(** set position to the eof **)
let set_end_exn (fd:Fd.t) : (unit Deferred.t) =
  set_offset_exn fd ~mode:`End Int64.zero

(** set position to the start **)
let set_start_exn (fd:Fd.t) : (unit Deferred.t) =
  set_offset_exn fd ~mode:`Set Int64.zero

(** skip the header **)
let skip_header_exn (fd:Fd.t) : (unit Deferred.t) =
  set_offset_exn fd (Int64.of_int (block_size))

(** set the offset to the required record **)
let set_record_exn (fd:Fd.t) (rec_num:int option) : (unit Deferred.t) =
  let (mode,offset) = (match rec_num with
    | None -> (`End,Int64.zero)
    | Some rec_num -> (`Set, Int64.( * ) (Int64.of_int rec_num) (Int64.of_int block_size))
  ) in
  set_offset_exn fd ~mode offset

let read_marshal (reader:Reader.t) : ([ `Eof of int | `Ok of 'a] Deferred.t) =
  let buff = String.create block_size in
  Reader.really_read reader ~len:block_size buff >>= function
    | `Eof i -> return (`Eof i)
    | `Ok -> return (`Ok (Marshal.from_string buff 0))

let read_header_marshal (reader:Reader.t) : 
  ([`Eof of int |`Ok of mbox_header] Deferred.t) =
  read_marshal reader >>= function
    | `Eof i -> return (`Eof i)
    | `Ok header -> return (`Ok header)

let read_record_marshal (reader:Reader.t) : 
  ([`Eof of int |`Ok of mbox_msg_metadata] Deferred.t) =
  read_marshal reader >>= function
    | `Eof i -> return (`Eof i)
    | `Ok record -> return (`Ok record)

(** index structure: 
  * header - 1024 bytes
  * each consequent record corresponding to the email message - 1024 bytes
  * the data is marshalled
**)
let read_mbox_header (file:string) : ([ `Eof of int | `Ok of mbox_header] Deferred.t) =
  Reader.with_file ~exclusive:true file 
  ~f:(fun r -> read_marshal r )

let write_mbox_header (file:string) (header:mbox_header) : unit Deferred.t =
  Unix_syscalls.with_file ~exclusive:`Write file 
  ~mode:[`Nonblock;`Wronly;`Creat]
  ~f:(fun fd ->
    let buff = String.create block_size in
    let i = Marshal.to_buffer buff 0 block_size header [Marshal.Compat_32] in
    if i > block_size then
      raise LongHeader
    else (
      let writer = Writer.create fd in
      Writer.write ~len:block_size writer buff; Writer.flushed writer >>= fun () -> set_end_exn fd
    )
  )

(** recursively read the index **)
let rec while_read_record (r:Reader.t) (cb:mbox_msg_metadata->unit) : ([`Eof of int] Deferred.t) =
  read_marshal r >>= function
    | `Eof i -> return (`Eof i)
    | `Ok record -> cb record; while_read_record r cb

type readerCb = (unit -> [`Header of mbox_header|`Record of mbox_msg_metadata|`Eof of int] Deferred.t)

(** call back argument for read_index_with_File **)
let index_reader (reader:Reader.t) (unit) :
  [`Header of mbox_header|`Record of mbox_msg_metadata|`Eof of int] Deferred.t =
  cur_offset (Reader.fd reader) >>= fun pos -> (** probably not expensive to get current position ?**)
  if pos = Int64.zero then
    read_header_marshal reader >>= function
    | `Eof i -> return (`Eof i)
    | `Ok header -> return (`Header header)
  else
    read_record_marshal reader >>= function
    | `Eof i -> return (`Eof i)
    | `Ok record -> return (`Record record)

(** read index file passing the record to the cb **)
let read_index_with_file (file:string) ?(skip:skipIndex option)
(cb:readerCb->'a Deferred.t) : 'a Deferred.t = 
  Unix_syscalls.with_file 
  ~exclusive:`Read 
  file
  ~mode:[`Nonblock;`Rdonly]
  ~f:(fun fd ->
    (match skip with
    | None -> return ()
    | Some skip -> 
      (match skip with
      | Header -> skip_header_exn fd 
      | Records n -> set_record_exn fd (Some (n+1)))
    ) >>= fun () ->
    let r = Reader.create fd in 
    (cb (index_reader r))
  )

(** read one record at specific position **)
let read_record (file:string) (recnum:int) : ([ `Eof of int | `Ok of 'a] Deferred.t) =
  Unix_syscalls.with_file ~exclusive:`Read file
  ~mode:[`Nonblock;`Rdonly]
  ~f:(fun fd -> set_record_exn fd (Some recnum) >>= fun () ->
    let r = Reader.create fd in
    read_marshal r
  )

(** open index for write, the Writer has to be closed 
 * this is continous write without lseek for each record
 * there is initial seek to skip the header
 **)
let open_index_record_write (file:string) : (Writer.t Deferred.t) =
    Unix_syscalls.openfile 
    file 
    ~mode:[`Nonblock;`Wronly;`Creat] >>= fun fd ->
      skip_header_exn fd >>= fun () -> return (Writer.create fd)

(** write marshalled metadata record **)
let write_record_marshal (writer:Writer.t) (record:mbox_msg_metadata) : unit Deferred.t =
  let buff = String.create block_size in
  let marshalled = Marshal.to_buffer buff 0 block_size record [Marshal.Compat_32] in
  if marshalled > block_size then
    raise LongRecord
  else
    Writer.write writer buff; Writer.flushed writer

(** write marshalled metadata record **)
let write_header_marshal (writer:Writer.t) (record:mbox_header) : unit Deferred.t =
  let buff = String.create block_size in
  let marshalled = Marshal.to_buffer buff 0 block_size record [Marshal.Compat_32] in
  if marshalled > block_size then
    raise LongRecord
  else
    Writer.write writer buff; Writer.flushed writer

(** close index writing **)
let close_index_record_write (writer:Writer.t) : unit Deferred.t =
  set_end_exn (Writer.fd writer) >>= fun () ->
  Unix_syscalls.close ~should_close_file_descriptor:true (Writer.fd writer)

(** write to index file specified record **)
let write_record (file:string) (recnum:int option) (record:mbox_msg_metadata) : (unit Deferred.t) =
  printf "write_record\n%!";
  Unix_syscalls.with_file ~exclusive:`Write file
  ~mode:[`Nonblock;`Wronly]
  ~f:(fun fd ->
    set_record_exn fd recnum >>= fun() ->
      let w = Writer.create fd in
      write_record_marshal w record >>= fun () -> set_end_exn fd
  )
	
(** write empty header **)
let write_empty_mbox_header (file:string) : (unit Deferred.t)=
  write_mbox_header file (empty_mbox_header())

(** create index file **)
let create_index_file (file:string) : (bool Deferred.t) =
  let folder = Filename.dirname file in
  (**
  Unix_syscalls.mkdir ~p:() ~perm:0o777 folder >>= fun() ->
    **)
  Unix_syscalls.system_exn("mkdir -p " ^ (Regex.squote folder)) >>= fun() ->
  Unix_syscalls.system_exn( "chmod 777 " ^ (Regex.squote folder)) >>= fun() ->
  Utils.file_exists file >>= fun e ->
  if e = false then
    Unix_syscalls.system_exn ("touch " ^ (Regex.squote file)) >>= fun () -> 
    Unix_syscalls.system_exn ("chmod 666 " ^ (Regex.squote file)) >>= fun () -> return true
  else
      return true
						
(** initialize mbox, create folder if needed **)
let init_mbox_header (file:string) : (bool Deferred.t) =
  let folder = Filename.dirname file in
  Utils.file_exists folder >>= fun e ->
  if e = false then (
    Utils.mkdir ~perm:0o777 ~parent:true folder >>= fun res ->
    if res then
      write_empty_mbox_header file >>= fun() -> return true
    else
      return false
  ) else (
    Utils.file_exists file >>= fun res ->
    if res then
      return true
    else
      write_empty_mbox_header file >>= fun () -> return true
  )

(** keep on writing to the index file until cb returns `Eof **)
let rec while_write_record (w:Writer.t) (cb: (unit -> [`Ok of mbox_msg_metadata|`Eof])) : (unit Deferred.t) =
  match (cb()) with
  | `Ok record -> write_record_marshal w record >>= fun () -> while_write_record w cb
  | `Eof -> return ()

(** write to the index file the header/record returned by the cb's (`Ok record | `Eof )
 this is sequential write **)
let write_with_file (file:string) ?(cb_header:mboxHeaderCb option)
~(cb_record:(unit->[`Ok of mbox_msg_metadata|`Eof])) : (unit Deferred.t) =
  printf "write_with_file\n%!";
  Unix_syscalls.with_file 
  ~exclusive:`Write 
  file
  ~mode:[`Nonblock;`Wronly]
  ~f:(fun fd ->
    let w = Writer.create fd in
    (match cb_header with
    | Some cb -> let header = cb() in write_header_marshal w header
    | None -> skip_header_exn fd
    ) >>= fun () -> while_write_record w cb_record
  )

(** writes to the index file header/record, the call back passed in
 * takes as argument a callback returning the record to write to the
 * file. when done the outer callback returns the header to write
 * to the file
 **)
let write_with_file_on_cb (file:string) (append:bool) 
  (cb_record:writeRecordCb -> (mbox_header,string) Result.t Deferred.t) : ((mbox_header,string) Result.t Deferred.t) =
  printf "write_with_file_on_cb\n%!";
  Unix_syscalls.with_file 
  ~exclusive:`Write 
  file
  ~mode:[`Nonblock;`Wronly]
  ~f:(fun fd ->
    (if append then
      set_end_exn fd
    else
      skip_header_exn fd) >>= fun () ->
    let writer = Writer.create fd in
    cb_record (write_record_marshal writer) >>= function
      | Ok header -> 
          set_start_exn fd >>= fun () ->
          write_header_marshal writer header >>= fun () -> 
          set_end_exn fd >>= fun() -> return (Ok header)
      | Error e -> return (Error e)
  )

(** call back takes header and record, returns header with accumulated header and
 * updated record to write back **)
let rec read_write_index seq (header:mbox_header) (reader:Reader.t) (writer:Writer.t)
(cb:(int->mbox_header->mbox_msg_metadata->(mbox_header*mbox_msg_metadata))) :
  [`Error of string|`Ok of mbox_header] Deferred.t =
  read_record_marshal reader >>= function
    | `Eof i -> 
      (if i = 0 then
        return (`Ok header)
      else
        return (`Error "Invalid Index")
      )
    | `Ok record -> set_record_exn (Writer.fd writer) (Some seq) >>= 
      fun () -> return (cb seq header record) >>= 
      fun (header,record) -> write_record_marshal writer record >>= 
      fun() -> read_write_index (seq+1) header reader writer cb

(** reads/writes to the index file header/record, the call back passed in
 * takes as argument a callback returning the record to write to the
 * file. when done the outer callback returns the header to write
 * to the file
 **)
let read_write_with_file_on_cb (file:string)
  (cb:(int->mbox_header->mbox_msg_metadata->(mbox_header*mbox_msg_metadata))) : 
  ((unit,string) Result.t Deferred.t) =
  Unix_syscalls.with_file 
  ~exclusive:`Write
  file
  ~mode:[`Nonblock;`Rdwr]
  ~f:(fun fd ->
    let reader = Reader.create fd in
    read_header_marshal reader >>= function
    | `Eof i -> return (Error "Invalid Index")
    | `Ok header ->
      let writer = Writer.create fd in
      read_write_index 1 header reader writer cb >>= function
      | `Ok header -> 
          set_start_exn fd >>= fun () ->
          write_header_marshal writer header >>= fun () -> 
          set_end_exn fd >>= fun() -> return (Ok())
      | `Error e -> return (Error e)
  )
	
let rec do_print the_reader =
  try_with (fun() ->
  the_reader () >>= function
      | `Eof i -> printf "done\n%!"; return()
      | `Header header -> print_header header; do_print the_reader
      | `Record record -> print_metadata record; do_print the_reader
  ) >>= fun res -> return()

(** print index file **)
let print_index (file:string) : (unit Deferred.t)  =
  printf "print_index %s\n%!" file;
  read_index_with_file file (fun the_reader -> do_print the_reader )
