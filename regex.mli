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

exception InvalidDate

val match_regex : ?case:bool -> string ->string -> bool

val match_regex_d : ?case:bool -> string ->string -> bool

val match_regex_i : ?case:bool -> string ->string -> int

val replace : string -> string -> string -> string

val date_time_regex : string

val date_time_dqregex : string

val day_of_week : int -> string

val int_to_month : int -> string

val of_day_of_week : string -> int

val month_to_int : string -> int

val append_regex : string

val imapd_to_date_exn : string -> Date.t

val imapd_to_date_time_exn : string -> Time.t

val date_time_to_email : Time.t -> string

val email_to_date_time_exn : string -> Time.t

(** match reserved files  **)
val match_dot : string -> bool 

(** convert imap mailbox regex to ocaml regex **)
val fixregx_mbox : string -> string

val list_of : string -> string

val dlist_of : string -> string

val bkt_list_of : string -> string

val ang_list_of : string -> string

val orx : string -> string -> string

val orxl : string list -> string

val astring : string

val group : string -> string

val dot : string

val sol : string

val eol : string

val all_of_it : string -> string

val number : string

val nz_number : string

val quote : string -> string

val squote : string -> string

val dequote : string -> string

val space : string

val optional : string -> string

val trim_space : string -> string

val crlf : string
