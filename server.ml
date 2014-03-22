open Core.Std
open Async.Std

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

(**
 * start accepting connections
**)
let create ~port ~host =
  Tcp.Server.create
  ~on_handler_error:`Raise
  (listen_on port host)
  (fun _addr r w -> Connection.client_connect r w) >>= fun _ -> never()
