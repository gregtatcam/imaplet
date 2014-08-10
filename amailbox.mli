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
open Storage
open StorageMeta
open Mflags

type t

exception BadMailbox of string

(** user account -> type **)
val create : string -> (Reader.t*Writer.t) option -> t

(** create empty mailbox **)
val empty : unit -> t 

(** is the mbox empty **)
val is_empty : t -> bool

(** mailboxes, parent folder, mailbox [wildcards] -> list of mailboxes/flags **)
val listmbx : t -> string -> string -> ((string*string list) list) Deferred.t

(** mailboxes, parent folder, mailbox [wildcards] -> list of subscribed mailboxes/flags **)
val lsubmbx : t -> string -> string -> ((string*string list) list) Deferred.t

(** select the mailbox **)
val select : t ->  string -> ([`NotExists|`NotSelectable|`Error of string|`Ok of t * mailbox_metadata] Deferred.t)

(** select the mailbox **)
val examine : t ->  string -> ([`NotExists|`NotSelectable|`Error of string|`Ok of t * mailbox_metadata] Deferred.t)

(** un-select the mailbox **)
val close : t -> t

(** create mailbox **)
val create_mailbox : t -> string -> [`Error of string|`Ok] Deferred.t

(** delete mailbox **)
val delete_mailbox : t -> string -> [`Error of string|`Ok] Deferred.t

(** rename mailbox **)
val rename_mailbox : t -> string -> string -> [`Error of string|`Ok] Deferred.t

(** subscribe mailbox **)
val subscribe : t -> string -> [`Error of string|`Ok] Deferred.t

(** subscribe mailbox **)
val unsubscribe : t -> string -> [`Error of string|`Ok] Deferred.t

(** get mailbox status **)
val get_status : t -> string -> States.statusOpt list -> 
  ([`NotExists|`NotSelectable|`Error of string|`Ok of mailbox_metadata] Deferred.t)

(** check if the mailbox is valid **)
val valid_mailbox : t -> string -> ([`NotExists|`NotSelectable|`ValidMailbox] Deferred.t)

(** append message to the mailbox **)
val append : t -> string -> Reader.t -> Writer.t -> mailboxFlags list option -> Time.t option ->
  States.literal -> ([`NotExists|`NotSelectable|`Eof of
  int|`Error of string|`Ok] Deferred.t)

(** sarch messages for the matching criteria **)
val search : t -> (States.searchKey) States.searchKeys -> bool ->
  [`NotExists|`NotSelectable|`Error of string|`Ok of int list] Deferred.t

val fetch : t -> (string->unit) -> States.sequence -> States.fetch -> bool ->
  [`NotExists|`NotSelectable|`Error of string|`Ok] Deferred.t

val store : t -> (string->unit) -> States.sequence -> States.storeFlags -> mailboxFlags
list -> bool -> [`NotExists|`NotSelectable|`Error of string|`Ok ] Deferred.t

val copy : t -> string -> States.sequence -> bool -> 
[`NotExists|`NotSelectable|`Error of string|`Ok ] Deferred.t 

val expunge : t -> (string->unit) -> [`Error of string|`Ok] Deferred.t 

val user : t -> string option

val selected_mbox : t -> string option
