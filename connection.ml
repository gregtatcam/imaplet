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

(** handle client connection **)

(** need to address multiple client connections **)

(** 
 * send capability response to the client
 * do pre-authentication if needed
 * return either Authenticated or Notauthenticated state
**)
let init_connection w =
  let resp = "* OK [CAPABILITY " ^ Configuration.capability ^ "] Imaplet ready.\r\n" in
  Writer.write w resp ~len:(String.length(resp));
  upon (Writer.flushed w) (fun() -> ());
  States.State_Notauthenticated

let init_storage () =
  let open IrminSrvIpc in
  match Configuration.get_store with
  | `Mbox | `Mailbox -> return None
  | `Irminsule -> get_irmin_server_ipc ()

(**
 * accepted client's connection
**)
let client_connect id connections r w =
  init_storage () >>= fun str_ipc ->
  let open Contexts in
  let state = init_connection w in
  let contexts = 
    ({req_ctx = (Contextlist.create()); state_ctx = state; mbx_ctx = Amailbox.empty() }) in
  let ipc_ctx = { connid = id; connections; logout_ctx = Condition.create(); net_r = r; net_w = w ; str_rw = str_ipc} in
  State.handle_client_requests contexts ipc_ctx
