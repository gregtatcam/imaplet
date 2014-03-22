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
