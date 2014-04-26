type t

(* block is a chunk of data in the container *)
val from_int : int -> t

(* block is a chunk of data in the container *)
val from_string : string -> t

(* update size *)
val update : t -> int -> t

(* get the block *)
val content : t -> string

(* return block size *)
val size : t -> int
