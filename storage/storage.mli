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

val empty_mailbox_metadata : ?uidvalidity:string -> unit -> mailbox_metadata

val empty_mailbox_message_metadata : unit -> mailbox_message_metadata

val empty_mbox_message_metadata : unit -> mbox_message_metadata

val update_mailbox_metadata: header:mailbox_metadata -> ?uidvalidity:string ->
  ?uidnext:int -> ?modseq:int64 -> ?count:int -> ?unseen:int -> ?nunseen:int -> ?recent:int -> unit -> mailbox_metadata

val update_mailbox_message_metadata : data:mailbox_message_metadata -> ?uid:int -> ?modseq:int64 ->
  ?internal_date:Time.t -> ?size:int ->
  ?flags:(mailboxFlags list) -> unit -> mailbox_message_metadata

val update_mbox_message_metadata : data:mbox_message_metadata -> ?uid:int -> ?modseq:int64 ->
  ?internal_date:Time.t -> ?size:int ->
  ?start_offset:int64 -> ?end_offset:int64 -> ?flags:(mailboxFlags list) -> unit
  -> mbox_message_metadata

val new_uidvalidity : unit -> string

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

module type StorageAccessor_intf = 
  sig
    type a
    type t
    type blk

    (* create accessor *)
    val create : a -> t 

    (* read block from the storage at requested position,
     * if position is none then sequential access,
     * position 0 is the header if applicable
     *)
    val reader : t -> [`Position of int] -> 
      [`Ok of blk|`Eof|`OutOfBounds] Deferred.t

    (* write block to the storage at requested position, 
     * If `OutOfBounds then requested position exceeds storage size
     * If `InvalidBlock then the block size is invalid
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

module UnixMboxIndexStorageAccessor : MboxIndexStorageAccessor_intf with type t =
  (UnixAccessor.t*int*int) and type a = (UnixAccessor.t*int*int)

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

val build_accs_inst :
  ( module MailboxAccessor_intf with 
    type a = 'a) -> 'a -> 
    (module StorageAccessor_inst)

module UnixMboxMailboxStorageAccessor : MailboxAccessor_intf with type t =
  (UnixMboxIndexStorageAccessor.t*UnixAccessor.t) and 
  type a = (UnixMboxIndexStorageAccessor.t*UnixAccessor.t)

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
     * dir - location for the mailbox and index root directories if different
     * (mainly applies to inbox: /var/mail*/Users/user/mail)
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

module UnixMboxIndexStorage : MboxIndexStorage_intf with 
  type loc = BasicLocation.t and 
  type param=int*int and 
  type t = BasicLocation.t*int*int and 
  type accs = UnixMboxIndexStorageAccessor.t

module UnixMboxMailboxStorage : MailboxStorage_intf with 
  type loc = BasicLocation.t and 
  type param=int*int and 
  type t = BasicLocation.t*BasicLocation.t*BasicLocation.t*int*int and
  type accs = UnixMboxMailboxStorageAccessor.t

val build_strg_inst :
  (module MailboxStorage_intf with 
    type loc = 'a and 
    type param = 'b and
    type accs = 'c) ->
  ('a*'a*'a*'b) -> ?tp2:'a*'a*'a*'b -> unit -> (module Storage_inst with type MailboxStorage.accs = 'c )