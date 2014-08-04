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

let marshal a =
    Marshal.to_string a [Marshal.Compat_32]

let launch_irmin_server () =
  Unix_syscalls.fork_exec
  ~prog:(Configuration.irmin_srv_exec)
  ~args:[Configuration.irmin_srv_exec] ()
  >>= fun _ -> after (Core.Span.create ~sec:1 ())

let get_irmin_server_ipc () =
  let open IrminStorageCmd in
  let open ServerConfig in
  try_with (fun () ->
    let where = Tcp.to_host_and_port (srv_config.irmin_addr)
    (srv_config.irmin_port) in
    Tcp.connect where >>= fun (_,r,w) -> 
    Writer.write w (marshal (Sexp.to_string (sexp_of_irminRequest `Bootstrap))) ; 
    Writer.flushed w >>= fun () ->
    Reader.read_marshal r >>= function 
    | `Ok _ -> printf "boostrapped\n%!"; return (Some (r,w))
    | `Eof -> printf "failed to bootstrap the store: eof\n%!"; return None
 ) >>= function
 | Ok res -> return res
 | Error _ -> return None

let close_irmin_server_ipc = function
  | None -> return ()
  | Some (r,w) -> 
  Reader.close r >>= fun () ->
  Writer.close w
