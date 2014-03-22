open Core.Std
open Async.Std

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
  size: int;
  start_offset: int64;
  end_offset: int64;
  flags: string list;
}

type skipIndex = Header|Records of int

type writeRecordCb = (mbox_msg_metadata -> unit Deferred.t)

type mboxHeaderCb = (unit -> mbox_header)

type readerCb = (unit -> [`Header of mbox_header|`Record of mbox_msg_metadata|`Eof of int] Deferred.t)

val init_mbox_header : string -> bool Deferred.t

val read_mbox_header : string -> ([ `Eof of int | `Ok of mbox_header] Deferred.t)

val write_mbox_header : string -> mbox_header -> unit Deferred.t

val read_record : string -> int -> ([ `Eof of int | `Ok of 'a] Deferred.t)

val write_record :string -> int option ->  mbox_msg_metadata -> unit Deferred.t

val open_index_record_write :string -> Writer.t Deferred.t

val write_record_marshal : Writer.t -> mbox_msg_metadata -> unit Deferred.t

val close_index_record_write : Writer.t -> unit Deferred.t

val empty_mbox_header : ?uidvalidity:string -> unit -> mbox_header

val empty_mbox_msg_metadata : unit -> mbox_msg_metadata

val update_mbox_header: header:mbox_header -> ?uidvalidity:string ->
  ?uidnext:int -> ?count:int -> ?unseen:int -> ?nunseen:int -> ?recent:int -> unit -> mbox_header

val update_msg_metadata : data:mbox_msg_metadata -> ?uid:int ->
  ?internal_date:Time.t -> ?size:int ->
  ?start_offset:int64 -> ?end_offset:int64 -> ?flags:(string list) -> unit -> mbox_msg_metadata

val new_uidvalidity : unit -> string

val print_header : mbox_header -> unit

val print_metadata: mbox_msg_metadata -> unit

val print_index : string -> unit Deferred.t

val cur_offset : (Fd.t) -> (int64 Deferred.t)

val read_index_with_file : string -> ?skip:skipIndex -> (readerCb->'a Deferred.t) -> 'a Deferred.t

val  write_with_file :string -> ?cb_header:mboxHeaderCb -> cb_record:(unit->[`Ok of mbox_msg_metadata|`Eof]) -> (unit Deferred.t) 

val write_with_file_on_cb : string -> bool -> (writeRecordCb -> (mbox_header,string) Result.t Deferred.t) -> 
  (mbox_header,string) Result.t Deferred.t

val read_write_with_file_on_cb : string -> 
  (int->mbox_header->mbox_msg_metadata->(mbox_header*mbox_msg_metadata)) -> 
    (unit,string) Result.t Deferred.t

val create_index_file :string -> bool Deferred.t

val read_header_marshal : (Reader.t) -> ([`Eof of int |`Ok of mbox_header] Deferred.t) 

val read_record_marshal : (Reader.t) -> ([`Eof of int |`Ok of mbox_msg_metadata] Deferred.t) 
