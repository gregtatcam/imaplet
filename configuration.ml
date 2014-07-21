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
open Utils

let revision = "IMAP4rev1"
let literal = "LITERAL+"
let sasl = "SASL-IR"
let login'ref = "LOGIN-REFERRALS"
let id = "ID"
let enable = "ENABLE"
let idle = "IDLE"
let starttls = "STARTTLS"
let auth = "AUTH=PLAIN"
let login'disabled = "LOGINDISABLED"

let capability () = let l = [revision;literal; sasl; login'ref; id; enable;
idle;starttls; auth] in
  List.fold_left l ~init:"" ~f:(fun all cap -> if all = "" then cap else all ^
  sp() ^ cap)

(* mbox index header/record size *)
let mbox_index_params () = (2048,1024)

let id () = "\"name\" \"Imaplet\""

let max_message_size =10_000_000

let max_message_in_memory_size = 0 (**10_240**)

let inbox_refresh () = 10

let inbox_root () =
  "/var/mail"

let inbox name =
  Filename.concat (inbox_root()) name

let mailboxes name = 
  "/Users/" ^ name ^ "/mail"

let get_mbox_flags =
  (["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"],
  ["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"; "\\*"])

let get_store () =
  `Irminsule

(* Irminsule *)
let irmin_srv_addr () = "localhost"

let irmin_srv_port () = 20001

let irmin_srv_exec () = "./irminStorageSrv.native"

let irmin_inbox_root () = "/"

let irmin_mailboxes () = "/"

(* lmtp *)
let lmtp_srv_exec = "./lmtp.native"
