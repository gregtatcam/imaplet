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
