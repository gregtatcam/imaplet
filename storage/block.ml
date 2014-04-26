open Core.Std

type t = int * string

(* block is a chunk of data in the container *)
let from_int sz = 
  (sz,String.create sz)

(* block is a chunk of data in the container *)
let from_string str = 
  (String.length str,str)

(* update block sz *)
let update blk sz = 
  let (i,s) = blk in
  if sz > i then
    (sz,String.concat [s;String.create (sz - i)])
  else if i = 0 then
    (0,String.create 0)
  else
    (sz,String.slice s 0 sz)

(* get the block string *)
let content blk = let (_,str) = blk in str

(* return block sz *)
let size blk = let (i,_) = blk in i
