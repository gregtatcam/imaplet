open Async.Std
open Email_message
open States
open Mflags

exception InvalidSequence

val exec_search : Email.t -> (searchKey) searchKeys -> Storage.mailbox_message_metadata -> int -> bool

val get_sequence : string -> (seq_set list)

val exec_fetch : int -> States.sequence -> Mailbox.Message.t ->
  Storage.mailbox_message_metadata -> States.fetch -> bool -> string option

val exec_store : Storage.mailbox_message_metadata->int -> States.sequence ->
  States.storeFlags -> mailboxFlags list -> bool -> 
    [`Ok of Storage.mailbox_message_metadata*string|`Silent of Storage.mailbox_message_metadata|`None]

val exec_seq : States.sequence -> int -> bool
