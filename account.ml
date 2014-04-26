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
open ExtLib
open States
open Utils

(**
 CAPABILITY IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE IDLE SORT
 SORT=DISPLAY THREAD=REFERENCES THREAD=REFS THREAD=ORDEREDSUBJECT MULTIAPPEND
 URL-PARTIAL CATENATE UNSELECT CHILDREN NAMESPACE UIDPLUS LIST-EXTENDED
 I18NLEVEL=1 CONDSTORE QRESYNC ESEARCH ESORT SEARCHRES WITHIN CONTEXT=SEARCH
 LIST-STATUS SPECIAL-USE BINARY MOVE
**)

(** users file:
  * dovecot:{PLAIN}dovecot:/Users/dovecot:/var/mail/dovecot
**)
let auth_capability () = "IMAP4rev1 LITERAL+ SASL-IR LOGIN-REFERRALS ID ENABLE IDLE"

let auth_user writer user password resp_ok resp_no =
  authenticate_user user password >>= fun res ->
  if res then
    (Response.write_resp writer (Resp_Untagged (Utils.formated_capability(auth_capability())));
    return (Ok (Resp_Ok (None,resp_ok), user)))
  else
    return (Error (Resp_No (None,resp_no)))

let plain_auth writer text =
  match (parse_user_b64 text) with
  | Some (u,p) -> auth_user writer u p "AUTHENTICATE" "PASSWORD"
  | None -> return (Error (Resp_No (None,"PASSWORD")))

(** TBD authenticate plain against users file **)
let authenticate writer auth_type text =
  printf "authenticating %s----\n%!" text;
  match auth_type with 
    | Auth_Plain -> plain_auth writer text
    | _ -> return (Error (Resp_No (None,"Authentication Type")))

(** TBD **)
let login writer user password = auth_user writer user password "LOGIN" "PASSWORD"
