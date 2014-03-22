open Async.Std

val client_connect : Reader.t -> Writer.t -> unit Deferred.t
