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
  str_rw : (Reader.t * Writer.t) option;
}
