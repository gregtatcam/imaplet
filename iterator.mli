open States

module type SequenceIterator_intf = 
  sig
    type t

    (* sequence -> max in seq *)
    val create : sequence -> int -> int -> t

    val single : sequence -> bool

    val next : t -> [`Ok of int|`End]

  end  

module SequenceIterator : SequenceIterator_intf with 
    type t = sequence * int ref*int ref*int ref*int * int
