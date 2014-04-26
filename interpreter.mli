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
open Async.Std
open Email_message
open States
open Mflags

exception InvalidSequence

val exec_search : Email.t -> (searchKey) searchKeys -> Storage.mailbox_message_metadata -> int -> bool

val get_sequence : string -> (seq_set list)

val exec_fetch : int -> States.sequence -> Mailbox.Message.t ->
  Storage.mailbox_message_metadata -> States.fetch -> bool -> string option

val exec_store : Storage.mailbox_message_metadata->int -> States.sequence ->
  States.storeFlags -> mailboxFlags list -> bool -> 
    [`Ok of Storage.mailbox_message_metadata*string|`Silent of Storage.mailbox_message_metadata|`None]

val exec_seq : States.sequence -> int -> bool
