(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gregtsip@cam.ac.uk>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
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
