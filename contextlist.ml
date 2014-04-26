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
open States
open Contexts

type t = request_context Stack.t

let create() = Stack.create()

let push t a =
  Stack.push t a

let pushs t a =
  Stack.push t a;
  t

let pop t =
  Stack.pop t

let peek t =
  Stack.top t

let length t =
  Stack.length t

let is_idle ctx =
  match ctx.req with 
  | Authenticated a ->
    (match a with 
    | Cmd_Idle -> true
    | _ -> false
    )
  | _ -> false

let is_done ctx =
  match ctx.req with 
  | Authenticated a ->
    (match a with 
    | Cmd_Done -> true
    | _ -> false
    )
  | _ -> false

let find_idle t =
  Stack.find t ~f:(fun ctx -> is_idle ctx.request)

let remove_idle t =
  Stack.fold t ~init:(Stack.create()) ~f:(
    fun ac ctx -> if is_idle ctx.request then ac else (push ac ctx;ac))
