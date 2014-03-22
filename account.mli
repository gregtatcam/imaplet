open Core.Std
open Async.Std

val authenticate : Writer.t -> States.authtype -> string ->
  (States.response * string, States.response) Result.t Deferred.t

val login : Writer.t -> string -> string ->
  (States.response * string, States.response) Result.t Deferred.t
