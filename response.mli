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

module StatusResponse :
  sig
    type response_type = Ok | Bad | No | Preauth | Bye
    val ok :
      ?tag:string -> ?code:States.responseCode option -> string -> string
    val bad :
      ?tag:string -> ?code:States.responseCode option -> string -> string
    val no :
      ?tag:string -> ?code:States.responseCode option -> string -> string
    val preauth : ?code:States.responseCode option -> string -> string
    val bye : ?code:States.responseCode option -> string -> string
    val untagged : string -> string
    val any : string -> string
    val continue : ?text:string -> unit -> string
  end

val write_resp : Writer.t -> ?tag:string -> States.response -> unit

val write_resp_untagged : Writer.t -> string -> unit
