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

let capability = let l = [revision;literal; sasl; login'ref; id; enable;
idle;starttls; auth] in
  List.fold_left l ~init:"" ~f:(fun all cap -> if all = "" then cap else all ^
  " " ^ cap)

(* mbox index header/record size *)
let mbox_index_params = (2048,1024)

let id = "\"name\" \"Imaplet\""

let max_message_in_memory_size = 0 (**10_240**)

let inbox_root = 
  let open ServerConfig in 
  srv_config.inbox_path

let inbox name =
  Filename.concat (inbox_root) name

let mailboxes name = 
  let open ServerConfig in 
  let l = String.split srv_config.mail_path ~on:'@' in
   (List.nth_exn l 0) ^ name ^ (List.nth_exn l 1)

let get_mbox_flags =
  (["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"],
  ["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"; "\\*"])

let get_store =
  `Irminsule

(* Irminsule *)
let irmin_srv_exec = Install.irmin_srv_exec

let irmin_inbox_root = "/"

let irmin_mailboxes = "/"

let irmin_backlog = 10

(* lmtp *)
let lmtp_srv_exec = Install.lmtp_srv_exec

let lmtp_backlog = 10

(* imap proxy *)
let prx_srv_exec = Install.prx_srv_exec

let srv_config_path = Install.config_path
