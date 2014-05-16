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
    val reader : t -> [`Position of int] -> [`Ok of blk|`Eof] Deferred.t

    (* write block to the storage at requested position, 
     *)
    val writer : t -> [`Append|`Position of int] -> blk -> [`Ok|`Eof] Deferred.t
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

module UnixMboxIndexStorageAccessor : MboxIndexStorageAccessor_intf with type t =
  (UnixAccessor.t*int*int) and type a = (UnixAccessor.t*int*int)

type mailbox_data = Mailbox.Message.t * mailbox_message_metadata

module type MailboxAccessor_intf = 
  sig
    include StorageAccessor_intf with type blk := mailbox_data

    val reader : t -> ?filter:(States.searchKey) States.searchKeys -> 
      [`Position of int] -> [`Ok of mailbox_data|`Eof|`NotFound] Deferred.t

    val writer : t -> [`Append] -> mailbox_data -> [`Ok] Deferred.t

    val reader_metadata : t -> [`Position of int] ->
      [`Ok of mailbox_message_metadata|`Eof] Deferred.t

    val writer_metadata : t -> mailbox_message_metadata -> [`Position of int] ->
      [`Ok |`Eof] Deferred.t
  end

module type StorageAccessor_inst = 
  sig
    module StorageAccessor : MailboxAccessor_intf
    val this : StorageAccessor.t
  end

val build_accs_inst :
  ( module MailboxAccessor_intf with 
    type a = 'a and type t = 'a) -> 'a -> 
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
      f:('a -> [`Folder of string*int|`Storage of string] -> 'a Deferred.t) -> 'a Deferred.t

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

    (* copy filtered *)
    val copy_with : t -> t -> filter:(bool*States.sequence) -> [`Ok|`SrcNotExists|`DestExists] Deferred.t

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
  ('a*'a*'a*'b) -> ?tp2:'a*'a*'a*'b -> unit -> (module Storage_inst)
(*
val build_strg_inst :
  (module MailboxStorage_intf with 
    type loc = 'a and 
    type param = 'b and
    type accs = 'c) ->
  ('a*'a*'a*'b) -> ?tp2:'a*'a*'a*'b -> unit -> (module Storage_inst with type MailboxStorage.accs = 'c )
  *)
