open Core.Std
open Async.Std
open Async_unix

let marshal a =
    Marshal.to_string a [Marshal.Compat_32]

let launch_irmin_server () =
  Unix_syscalls.fork_exec
  ~prog:(Configuration.irmin_srv_exec())
  ~args:[Configuration.irmin_srv_exec()] ()
  >>= fun _ -> after (Core.Span.create ~sec:1 ())

let get_irmin_server_ipc () =
  let open IrminStorageCmd in
  try_with (fun () ->
    let where = Tcp.to_host_and_port (Configuration.irmin_srv_addr())
    (Configuration.irmin_srv_port()) in
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
