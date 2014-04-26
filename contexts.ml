open Async.Std
open Async_kernel

type ipc_message = [`Logout]

type client_request = {
  tag : string;
  req : States.fromClient
}

type context = {
  ctx_buf : string
}

type request_context = {
  request : client_request;
}

type ('a, 'b) exec_context = {
  req_ctx : 'a;
  state_ctx : States.state;
  mbx_ctx : 'b ;
}

type ('b) resp_context = {
  resp_state_ctx : States.state option;
  resp_ctx : States.response;
  resp_mbx_ctx : 'b option;
}

type ipc_context = {
  logout_ctx : unit Async_condition.t;
  net_r : Reader.t;
  net_w : Writer.t;
}
