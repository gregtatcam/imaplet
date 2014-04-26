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

val send : Writer.t -> string -> unit

val send_wcrlf : Writer.t -> string -> unit

val sp : unit -> string

val star : unit -> string

val crlf : unit -> string

val with_crlf : string -> string

val to_qstr : string -> string

val to_qnv : string -> string -> string

val to_plist : string -> string

val to_blist : string -> string

val formated_capability : string -> string

val formated_id : string -> string

val  parse_users : string -> string -> string -> bool

val read_users : Reader.t -> string -> string -> bool Deferred.t

val authenticate_user : ?users:string -> string -> string -> bool Deferred.t

val parse_user_b64 : string -> (string * string) option

val read_literal : Reader.t -> int -> ([`Eof | `Ok ] -> string -> unit) -> [`Eof of int | `Ok] Deferred.t

(** create folder **)
val create_folder : perm:Unix.file_perm -> string  -> bool Deferred.t 

(** delete an empty folder **)
val delete_folder : string  -> bool Deferred.t

(** create directory with parents option **)
val mkdir : perm:Unix.file_perm -> ?parent:bool -> string -> bool Deferred.t

(** delete a file **)
val delete_file : string -> bool Deferred.t

(** rename a file/folder **)
val rename : string -> string -> bool Deferred.t

(** check if file exists **)
val file_exists : string -> bool Deferred.t

(** check if file is directory **)
val is_directory : string -> bool Deferred.t

val concat_path : string -> string -> string

val make_path : string list -> string

val substr : string -> start:int -> size:int option -> string
