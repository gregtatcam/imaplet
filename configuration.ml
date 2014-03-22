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

let id () = "\"name\" \"Imaplet\""

let max_message_size =10_000_000

let max_message_in_memory_size = 0 (**10_240**)

let inbox name =
  "/var/mail/" ^ name

let mailboxes name = 
  "/Users/" ^ name ^ "/mail"

let get_mbox_flags =
  (["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"],
  ["\\Answered"; "\\Flagged"; "\\Deleted"; "\\Seen"; "\\Draft"; "$NotJunk";
  "NotJunk"; "\\*"])
