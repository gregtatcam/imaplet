open Async.Std

module StatusResponse :
  sig
    type response_type = Ok | Bad | No | Preauth | Bye
    val ok :
      ?tag:string -> ?code:States.responseCode option -> string -> string
    val bad :
      ?tag:string -> ?code:States.responseCode option -> string -> string
    val no :
      ?tag:string -> ?code:States.responseCode option -> string -> string
    val preauth : ?code:States.responseCode option -> string -> string
    val bye : ?code:States.responseCode option -> string -> string
    val untagged : string -> string
    val any : string -> string
    val continue : ?text:string -> unit -> string
  end

val write_resp : Writer.t -> ?tag:string -> States.response -> unit

val write_resp_untagged : Writer.t -> string -> unit
