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
open Primitives
open StorageMeta
open Email_message

(* first is user *)
type irminRequest = [
  | `Bootstrap
  | `Copy_with of string*string * string*(bool*States.sequence)
  | `Create of string*string
  | `Create_account of string
  | `Delete of string*string
  | `Exists of string*string
  | `Expunge of string*string
  | `Get_subscription of string
  | `List_store of string*string
  | `Mailbox_metadata of string*string
  | `Move of string*string * string
  | `Reader of string*string * [`Position of int]*(States.searchKey) States.searchKeys option
  | `Reader_metadata of string*string*([`Position of int])
  | `Remove_account of string
  | `Rebuild_index of string*string
  | `Search_with of string*string*(bool*(States.searchKey) States.searchKeys)
  | `Subscribe of string * string
  | `Unsubscribe of string * string
  | `Update_index of string*string
  | `Writer of string*string*[`Append] * (Mailbox.Message.t * mailbox_message_metadata)
  | `Writer_metadata of string*string*[`Position of int] *mailbox_message_metadata
] with sexp

type irminResponse = [
  | `Bootstrap
  | `Copy_with of ([`Ok|`SrcNotExists|`DestExists])
  | `Create of ([`Storage|`Folder])
  | `Create_account of [`Ok|`Exists]
  | `Delete 
  | `Eof
  | `Exists of ([`No|`Storage|`Folder])
  | `Expunge 
  | `Get_subscription of string list
  | `List_store of ([`Folder of string*int|`Storage of string] list)
  | `Mailbox_metadata of mailbox_metadata
  | `Move 
  | `Reader of ([`Ok of Mailbox.Message.t*mailbox_message_metadata|`Eof|`NotFound])
  | `Reader_metadata of ([`Ok of mailbox_message_metadata|`Eof])
  | `Remove_account of [`Ok|`DoesntExist]
  | `Rebuild_index
  | `Search_with of int list
  | `Subscribe
  | `Unsubscribe
  | `Update_index of ([`NotExists|`Ok])
  | `Writer of ([`Ok]) 
  | `Writer_metadata of ([`Ok|`Eof])
] with sexp
