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
  let resp = "* OK [CAPABILITY " ^ Configuration.capability() ^ "] Imaplet ready.\r\n" in
  Writer.write w resp ~len:(String.length(resp));
  upon (Writer.flushed w) (fun() -> ());
  States.State_Notauthenticated

(**
 * accepted client's connection
**)
let client_connect r w =
  let open Contexts in
  let state = init_connection w in
  let contexts = 
    ({req_ctx = (Contextlist.create()); state_ctx = state; mbx_ctx = Amailbox.empty() }) in
  let ipc_ctx = { logout_ctx = Condition.create(); net_r = r; net_w = w } in
  State.handle_client_requests contexts ipc_ctx
