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

val match_regex : ?case:bool -> string ->string -> bool

val match_regex_i : ?case:bool -> string ->string -> int

(* primitives - abstraction for OS and/or specific storage API *)

type flags = 
| Append
| Creat
| Nonblock
| Rdonly
| Rdwr
| Trunc
| Wronly

(* open flags *)
module type Flags = 
  sig
    type t
    val create : flags list -> t
    val get : t -> flags list  
  end

module UnixFlags : Flags with type t = Unix_syscalls.open_flag list

(* storage location *)
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
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
  end

module BasicLocation : Location with type t = string

(* position into storage *)
module type Position = 
  sig
    type t
    val of_str : string -> t
    val of_int64 : Int64.t -> t
    val to_string : t -> string
    val to_int64 : t -> Int64.t
    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t
  end

module BasicPosition : Position with type t = Int64.t

(* open permissions *)
module type Permissions = 
  sig
    (* container permissions *)
    type t
    val create : int -> t
    val to_int : t -> int
  end

module BasicPermissions : Permissions with type t = int

(* locking *)
module type Lock = 
  sig
    type t
    type m
    val create : t -> t
    val lock : t -> m -> unit Deferred.t
    val unlock : t -> unit
  end 

module UnixLock : Lock with type t = Fd.t and type m = [`Read|`Write]

(* storage access: seek/rw *)
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

module UnixAccessor : Accessor with type t = (Reader0.t * Writer0.t) and type f
= Fd.t and type p = BasicPosition.t

(* storage manipulation *)
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
     * as list recurses into folders, the suffix is the relative path
     *)
    val list_store : start:loc -> suffix:loc -> ?exclude:string -> init:'a ->
      f:('a -> [`Folder of loc * int|`Storage of loc] -> 'a Deferred.t) -> 'a Deferred.t
    
    (* create storage if it doesn't exist *)
    val create : ?perm:perms -> loc -> [`Folder|`Storage] Deferred.t
    
    (* delete the storage *)
    val delete : loc -> unit Deferred.t
    
    (* move the storage to a new location *)
    val move : loc -> loc -> unit Deferred.t
    
    (* location exists and type *)
    val exists : loc -> [`No|`Folder|`Storage] Deferred.t
  end
  
module UnixUtils : Utils with type fl = UnixFlags.t and type loc =
  BasicLocation.t and type perms = BasicPermissions.t and type accs =
    UnixAccessor.t
