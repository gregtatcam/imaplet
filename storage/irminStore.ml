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
(* ocamlfind c -w A -linkpkg -package lwt,lwt.unix,lwt.syntax -syntax
  camlp4o,lwt.syntax myecho.ml -o myecho *)
open Lwt
 
let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())
 
let create_socket () =
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, IrminStoreConfig.irmin_server_port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket IrminStoreConfig.backlog;
  socket

let process_request outchan msg =
  Printf.printf "read msg %s\n%!" msg;
  Lwt_io.write_line outchan msg


let rec requests inchan outchan =
  catch (fun () -> 
  Lwt_io.read_line inchan >>= fun msg ->
  process_request outchan msg >>= fun () -> requests inchan outchan
  )
  (fun ex -> Printf.printf "connection closed %s\n%!" (Core.Exn.to_string ex);
  try_close inchan >> try_close outchan >> return() 
  )
 
let process socket =
  let rec _process () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
        async (fun () -> requests inchan outchan);
        _process ()
      )
  in
  _process ()
 
let _ =
  let socket = create_socket () in
  Lwt_main.run (
    process
      socket
  )
