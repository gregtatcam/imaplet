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

module IrminMailbox : IrminMailbox_intf with type t = string list
