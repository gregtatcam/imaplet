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
