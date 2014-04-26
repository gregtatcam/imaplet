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

(** do daemon steps if needed **)

(**
 * handle configuration options
**)
let handle_config = function
| Some c -> () (** TBD **)
| None -> ()

(**
 * handle command line
**)
let command =
  Command.basic
    ~summary:"run imaplet server"
      Command.Spec.(
      empty
      +> flag "-c" (optional file) ~doc:"configuration file(optional)"
      +> flag "-p" (optional_with_default 143 int) ~doc:"bind port(optional)"
      +> flag "-a" (optional string) ~doc:"bind address(optional)"
      )
      (fun c p h () -> 
        handle_config c; 
        upon (Server.create ~port:p ~host:h) (fun _ -> ());
        never_returns (Scheduler.go()))

(**
 * start the server
**)
let () = 
  try
    Command.run command
  with Exit ->
    Printexc.print_backtrace stderr
