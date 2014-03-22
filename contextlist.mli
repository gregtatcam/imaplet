type t

val create : unit -> t

val push : t -> Contexts.request_context -> unit

val pushs : t -> Contexts.request_context -> t

val pop : t -> Contexts.request_context option

val peek : t -> Contexts.request_context option

val length : t -> int

val is_idle : Contexts.client_request -> bool

val is_done : Contexts.client_request -> bool

val find_idle : t -> Contexts.request_context option

val remove_idle : t -> t
