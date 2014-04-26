(** un-authenticated capabilities **)
val capability : unit -> string

(** system id **)
val id : unit -> string

(** inbox refresh rate **)
val inbox_refresh : unit -> int

(** inbox root **)
val inbox_root : unit -> string

(** inbox folder **)
val inbox : string -> string

(** mailboxes folder **)
val mailboxes : string -> string

(** max allowed message size **)
val max_message_size : int

(** max message size to process in-memory **)
val max_message_in_memory_size : int

val get_mbox_flags : (string list*string list)

(* mbox index header/record size *)
val mbox_index_params : unit -> (int * int)
