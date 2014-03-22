open Core.Std
open Async.Std
open Async_unix


let spec =
let open Command.Spec in
empty
+> anon ("filename" %: string)

let printme f = Index.print_index f >>= fun res -> exit 0

let command =
    Command.basic
    ~summary:"print index file"
    spec
    (fun filename () -> 
      upon(try_with(fun()->printme filename)) (fun res -> 
        match res with
        |Error ex -> printf "error: %s\n%!" (Exn.to_string ex)
        |Ok _ -> printf "done\n%!");
    never_returns (Scheduler.go()))

let () = 
  try
    Command.run command
  with Exit ->
    Printexc.print_backtrace stderr
