open Async.Std
open States

module type SequenceIterator_intf = 
  sig
    type t

    (* sequence -> max in seq *)
    val create : sequence -> int -> int -> t

    val single : sequence -> bool

    val next : t -> [`Ok of int|`End]

  end  

module SequenceIterator : SequenceIterator_intf with type t = sequence*int
ref*int ref*int ref*int*int = 
  struct
    (* sequence, next element in sequence, current counter, max for the
       counter, overal min and max for the mailbox *)
    type t = sequence * int ref*int ref*int ref*int * int

    let create seq min max = (seq,ref 0, ref 0, ref 0, min,max)

    let single seq =
      if Core.Std.List.length seq = 1 then
        match (Core.Std.List.hd_exn seq) with
        | SeqNumber sn ->
          (
          match sn with
          | Number n -> true
          | Wild -> false
          )
        | SeqRange _ -> false
      else
        false

    let next t = 
      let s,nc,c,cmax,min,max = t in
      let get_n m = function
        | Number n -> n
        | Wild -> m
      in
      let update mi mx =
        c := mi;
        cmax := mx;
        `Ok !c
      in
      c := !c + 1;
      if !c > !cmax then (
        let seq = Core.Std.List.nth s !nc in
        match seq with
        | None -> `End
        | Some seq ->
          (
          nc := !nc + 1; (* ref to the next element in the sequence *)
          match seq with
          | SeqNumber sn ->
            (match sn with
            | Number n -> update n n
            | Wild -> update min max
            )
          | SeqRange (sn1,sn2) ->
              update (get_n min sn1) (get_n max sn2)
          )
      ) else
        `Ok !c
  end
