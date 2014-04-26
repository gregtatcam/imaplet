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
open Async.Std
open Async_unix

let match_regex_i ?(case=true) str regx =
try
  let regexp =
  (
  if case = false then
   Str.regexp_case_fold regx
  else
   Str.regexp regx
  ) in
  (Str.search_forward regexp str 0)
  with _ ->
   (-1)

let match_regex ?(case=true) str regx =
  let i = match_regex_i ~case str regx in
  (i >= 0)

type flags = 
| Append
| Creat
| Nonblock
| Rdonly
| Rdwr
| Trunc
| Wronly

(* primitives - abstraction for OS and/or specific storage API *)
module type Flags = 
sig
  type t
  val create : flags list -> t
  val get : t -> flags list
end

(* primitives - abstraction for OS and/or specific storage API *)
module type Location = 
sig
  type t
  val create : string -> t
  val to_str : t -> string
  val to_str_qt : t -> string
  val basename : t -> string
  val dirname : t -> string
  val concat : t -> t -> string
  val compare : t -> t -> int
end

module type Position = 
sig
  type t
  val of_str : string -> t
  val of_int64 : Int64.t -> t
  val to_string : t -> string
  val to_int64 : t -> Int64.t
  val compare : t -> t -> int
end

module type Permissions = 
sig
  (* container permissions *)
  type t
  val create : int -> t
  val to_int : t -> int
end

module type Lock = 
sig
  type t
  type m
  val create : t -> t
  val lock : t -> m -> unit Deferred.t
  val unlock : t -> unit
end 

module type Accessor = 
sig
  type t
  type f
  type p

  val create : f -> t

  val reader : t -> Block.t -> [`Ok of Block.t|`Eof of Block.t] Deferred.t

  val reader_line : t -> [`Ok of Block.t|`Eof] Deferred.t

  val writer : t -> Block.t -> unit Deferred.t

  val writer_line : t -> Block.t -> unit Deferred.t

  val seek_end : t -> p Deferred.t

  val seek_start : t -> p Deferred.t

  val seek_set : t -> p -> p Deferred.t

  val seek_current : t -> p Deferred.t

  val close : t -> unit Deferred.t
end

module type Utils = 
sig
  type fl
  type loc
  type perms
  type accs
  (* fold the storage *)
  val fold : loc -> ?exclusive:bool -> ?perm:perms -> ?flags:fl -> init:'a -> 
    f:('a -> accs -> 'a Deferred.t) -> 'a Deferred.t
  
  (* list storage content folder/mailbox, apply the filter,
   * start is initial location, and suffix is blank,
   * as list recurses into folders, the suffix is growing
   *)
  val list_store : start:loc -> suffix:loc -> ?exclude:string -> init:'a ->
    f:('a -> [`Folder of loc * int|`Storage of loc] -> 'a) -> 'a Deferred.t
  
  (* create storage if it doesn't exist *)
  val create : ?perm:perms -> loc -> [`Folder|`Storage] Deferred.t
  
  (* delete the storage *)
  val delete : loc -> unit Deferred.t
  
  (* move the storage to a new location *)
  val move : loc -> loc -> unit Deferred.t
  
  (* location exists and type *)
  val exists : loc -> [`No|`Folder|`Storage] Deferred.t
end

module UnixFlags = 
struct
  type t = Unix_syscalls.open_flag list
  let create fl = List.fold fl ~init:[] ~f:(fun acc f -> (match f with
    | Append -> `Append
    | Creat -> `Creat
    | Nonblock -> `Nonblock
    | Rdonly -> `Rdonly
    | Rdwr -> `Rdwr
    | Trunc -> `Trunc
    | Wronly -> `Wronly) :: acc )

  let get (fl:t) : (flags list) = List.fold fl ~init:[] ~f:(fun acc f -> 
    match f with
    | `Append -> Append :: acc
    | `Creat -> Creat :: acc
    | `Nonblock -> Nonblock :: acc
    | `Rdonly -> Rdonly :: acc
    | `Rdwr -> Rdwr :: acc
    | `Trunc -> Trunc :: acc
    | `Wronly -> Wronly :: acc
    | _ -> acc
  )
end

module BasicLocation =
struct
  type t = string

  (* create location *)
  let create location = location

  (* get string location *)
  let to_str location = location

  (* get quoted location *)
  let to_str_qt location = "\"" ^ location ^ "\""

  (* get location's basename *)
  let basename location = Filename.basename location

  (* get location's dirname *)
  let dirname location = Filename.dirname location

  (* concatenate *)
  let concat l1 l2  = 
    if l1 = "" then
      l1
    else
      Filename.concat l1 l2

  (* compare *)
  let compare l1 l2 = 
    String.compare l1 l2

end

module BasicPosition = 
struct
  type t = Int64.t
  
  (* create position *)
  let of_str str = Int64.of_string str

  let of_int64 i = i
  
  (* get position's string *)
  let to_string pos = Int64.to_string pos
  
  (* get position's int *)
  let to_int64 pos = pos

  (* compare *)
  let compare p1 p2 = 
    Int64.compare (to_int64 p1) (to_int64 p2)
end

module BasicPermissions = 
struct
  type t = int

  (* create permissions *)
  let create perm = perm

  (* get permissions *)
  let to_int perm = perm
end

module UnixLock = 
struct
  type t = Fd.t
  type m = [`Read|`Write]
  
  (* create the lock *)
  let create location = location 
  
  (* lock *)
  let lock lck mode = Unix_syscalls.lockf lck mode 
  
  (* unlock *)
  let unlock lck = Unix_syscalls.unlockf lck 
end

module UnixAccessor = 
struct
  type t = Reader0.t * Writer0.t
  type f = Fd.t
  type p = BasicPosition.t

  (* create Accessor *)
  let create fd =
    (Reader0.create fd,Writer0.create fd)

  (* read the block from the container *)
  let reader accs blk =
   let (r,_) = accs in
   Reader0.really_read r ~pos:0 ~len:(Block.size blk) (Block.content blk) >>= function
     | `Ok -> return (`Ok blk)
     | `Eof i -> return (`Eof (Block.update blk i))

  (* read the block from the container *)
  let reader_line accs =
   let (r,_) = accs in
   Reader0.read_line r >>= function
     | `Ok str -> return (`Ok (Block.from_string str))
     | `Eof -> return (`Eof)

  (* write the block to the container *)
  let writer accs blk =
   let (_,w) = accs in
   Writer0.write w ~pos:0 ~len:(Block.size blk) (Block.content blk);
   Writer0.flushed w

  (* write the block to the container *)
  let writer_line accs blk =
   let (_,w) = accs in
   Writer0.write_line w (Block.content blk);
   Writer0.flushed w

  (*
  let mode_to_string = function
    | `Cur -> "current"
    | `End -> "end"
    | `Set -> "set"
  *)

  (* seek to required position, return Position set *)
  let seek accs pos ~mode =
   let (r,_) = accs in
   Reader0.lseek r pos ~mode >>= fun pos ->
     return (BasicPosition.of_int64 pos)

  (* advance to the end of container *)
  let seek_end accs =
   seek accs Int64.zero ~mode:`End

  (* advance to the start of container *)
  let seek_start accs =
   seek accs Int64.zero ~mode:`Set

  (* set requested position *)
  let seek_set accs pos =
   seek accs (BasicPosition.to_int64 pos) ~mode:`Set

  (* get current position *)
  let seek_current accs =
   seek accs Int64.zero ~mode:`Cur

  let close accs =
   let (r,w) = accs in
   Reader0.close r >>= fun () ->
     Writer0.close w
end

module UnixUtils =
struct
  type fl = UnixFlags.t
  type loc = BasicLocation.t
  type perms = BasicPermissions.t
  type accs = UnixAccessor.t
  let def_perms = BasicPermissions.create 0o666
  let def_flags = [`Nonblock;`Rdwr]

(* create storage *)
  let create ?(perm=def_perms) location =
   let location = BasicLocation.to_str location in
   let qlocation = BasicLocation.to_str_qt location in
   let perm = BasicPermissions.to_int perm in
   let dirname =
     if String.nget location ((String.length location) - 1) = '/' then
       location
     else
       Filename.dirname location
   in
   Unix_syscalls.mkdir ~p:() ~perm:0o777 dirname >>= fun () ->
   if dirname <> location then
     Unix_syscalls.system_exn ("touch " ^ qlocation) >>= fun () ->
     Unix_syscalls.chmod location ~perm >>= fun() -> return `Storage
   else
     return `Folder

(* delete the storage *)
  let delete location =
   Unix_syscalls.system_exn ("rm -rf " ^ (BasicLocation.to_str_qt location))

(* move the storage to a new location *)
  let move src dest =
   Unix_syscalls.system_exn ("mv -f " ^ (BasicLocation.to_str_qt src) ^ " " ^
   (BasicLocation.to_str_qt dest))

  let exists location =
   let location = BasicLocation.to_str location in
   Sys.file_exists location >>= function
     | `No -> return `No
     | `Unknown -> return `No
     | `Yes -> Sys.is_directory location >>= function
       | `No -> return `Storage
       | `Unknown -> return `No
       | `Yes -> return `Folder

  (* fold container, call ~f with Accessor for navigation/access, return 'a when
  * done
  *)
  let fold location ?(exclusive=false) ?(perm=def_perms) ?(flags=def_flags) ~init ~f =
   let findfl flags flag =
     List.find flags ~f:(fun fl -> if fl = flag then true else false) <> None
   in
   let lockfl flags =
     if (findfl flags `Wronly) || (findfl flags `Rdwr) then
       `Write
     else
       `Read
   in
   Unix_syscalls.openfile ~perm:(BasicPermissions.to_int perm) (BasicLocation.to_str location) 
   ~mode:flags >>= fun fd ->
     (if exclusive = true then
       Unix_syscalls.lockf fd (lockfl flags)
     else
       return ()) >>= fun () ->
     let acs = UnixAccessor.create fd in
     f init acs >>= fun res ->
       UnixAccessor.close acs >>= fun () -> 
         Unix_syscalls.close ~should_close_file_descriptor:true fd >>= fun() -> return res

  let file_concat a b =
   if a = "" then
     b
   else
     Filename.concat a b
  
  (* get the list of folders/mailboxes, apply f to each content *)
  let list_store ~start ~suffix ?(exclude="") ~init ~f =
    (* wrapper to add count of items in each folder *)
    let rec list_store_ ~start ~suffix ~init ~f =
      let dir = Filename.concat (BasicLocation.to_str start) (BasicLocation.to_str suffix) in
      Sys.ls_dir dir >>= fun content ->
        Deferred.List.fold content ~init ~f:(fun (acc,cnt) basename ->
          let full_path = Filename.concat dir basename in
          let full_location = BasicLocation.create full_path in
          let rel_location = BasicLocation.create (file_concat (BasicLocation.to_str suffix) basename) in
          if exclude <> "" && match_regex rel_location exclude then
            return (acc,cnt)
          else
            exists full_location >>= function
            | `No -> return (acc,cnt)
            | `Storage -> return ((f acc (`Storage rel_location)),cnt + 1)
            | `Folder ->
                list_store_ ~start ~suffix:rel_location ~init:(acc,0) ~f >>= fun (acc,cnt) -> 
              return ((f acc (`Folder (rel_location,cnt))),cnt+1)
        )
    in
    list_store_ ~start ~suffix ~init:(init,0) ~f >>= fun (acc,_) -> return acc
end
