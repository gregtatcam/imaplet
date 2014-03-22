open Async.Std
open Email_message
open States

exception InvalidSequence

val exec_search : Email.t -> (searchKey) searchKeys -> Index.mbox_msg_metadata -> int -> bool

val get_sequence : string -> (seq_set list)

val exec_fetch : int -> States.sequence -> Mailbox.Message.t ->
  Index.mbox_msg_metadata -> States.fetch -> bool -> string option

val exec_store : Index.mbox_msg_metadata->int -> States.sequence ->
  States.storeFlags -> States.flags list -> bool -> 
    [`Ok of Index.mbox_msg_metadata*string|`Silent of Index.mbox_msg_metadata|`None]

val exec_seq : States.sequence -> int -> bool
