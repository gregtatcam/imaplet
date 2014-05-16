open Async.Std

val launch_irmin_server : unit -> unit Deferred.t

val get_irmin_server_ipc : unit -> (Reader.t*Writer.t) option Deferred.t

val close_irmin_server_ipc : (Reader.t*Writer.t) option -> unit Deferred.t
