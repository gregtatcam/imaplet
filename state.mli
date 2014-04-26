open Core.Std
open Async.Std
open States
open Contexts

val handle_client_requests : 
  (Contextlist.t, Amailbox.t) exec_context -> ipc_context -> unit Deferred.t
