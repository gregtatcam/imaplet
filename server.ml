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
open Async_unix

(**
 * listen on port/host
 * bind to any interface if no host
**)
let listen_on port host =
  let addr = match host with 
	| None -> UnixLabels.inet_addr_any 
	| Some host -> UnixLabels.inet_addr_of_string host
	in
    Tcp.Where_to_listen.create
    ~socket_type:Socket.Type.tcp 
    ~address:(`Inet (addr,port))
    ~listening_on:(fun _ -> port)

(* initialize storage *)
let init_storage () =
  let open IrminSrvIpc in
  match Configuration.get_store with
  | `Mbox | `Mailbox -> return ()
  | `Irminsule -> launch_irmin_server ()

(* init local delivery *)
let init_local_delivery () =
  Unix_syscalls.fork_exec
  ~prog:(Configuration.lmtp_srv_exec)
  ~args:[Configuration.lmtp_srv_exec] () >>= fun _ -> return ()

(* init front end server *)
let init_front_end () =
  Unix_syscalls.fork_exec
  ~prog:(Configuration.prx_srv_exec)
  ~args:[Configuration.prx_srv_exec] () >>= fun _ -> return ()

(* initialize all things *)
let init_all () =
  init_storage () >>= fun () ->
  init_local_delivery () >>= fun () ->
  init_front_end ()

let connections = ref []
let connid = ref Int64.zero

(**
 * start accepting connections
**)
let create ~port ~host =
  init_all () >>= fun () ->
  Tcp.Server.create
  ~on_handler_error:`Raise
  (listen_on port host)
  (fun _addr r w -> 
    connid := Int64.(+) !connid Int64.one;
    let id = !connid in
    Connection.client_connect id connections r w >>= fun _ -> 
    connections := List.fold !connections ~init:[] ~f:(fun acc i ->
      let (cid,_,_) = i in
      if Int64.equal cid id then
        acc
      else
        i :: acc
    );
    return ()
  ) >>= fun _ -> never()
