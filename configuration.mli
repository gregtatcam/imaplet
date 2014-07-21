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
(** un-authenticated capabilities **)
val capability : unit -> string

(** system id **)
val id : unit -> string

(** inbox refresh rate **)
val inbox_refresh : unit -> int

(** inbox root **)
val inbox_root : unit -> string

(** inbox folder **)
val inbox : string -> string

(** mailboxes folder **)
val mailboxes : string -> string

(** max allowed message size **)
val max_message_size : int

(** max message size to process in-memory **)
val max_message_in_memory_size : int

val get_mbox_flags : (string list*string list)

(* mbox index header/record size *)
val mbox_index_params : unit -> (int * int)

(* get type of storage *)
val get_store : unit -> [`Mbox|`Mailbox|`Irminsule] 

val irmin_srv_addr : unit -> string

val irmin_srv_port : unit -> int

val irmin_srv_exec : unit -> string

val irmin_inbox_root : unit -> string

val irmin_mailboxes : unit -> string

val lmtp_srv_exec : string
