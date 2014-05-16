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
  | `Reader of string*string * [`Position of int]
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
  | `Reader of ([`Ok of Mailbox.Message.t*mailbox_message_metadata|`Eof|`OutOfBounds])
  | `Reader_metadata of ([`Ok of mailbox_message_metadata|`Eof|`OutOfBounds])
  | `Remove_account of [`Ok|`DoesntExist]
  | `Rebuild_index
  | `Search_with of int list
  | `Subscribe
  | `Unsubscribe
  | `Update_index of ([`NotExists|`Ok])
  | `Writer of ([`Ok|`InvalidBlock]) (* should remove InvalidBlock in storage.ml[i] TBD *)
  | `Writer_metadata of ([`Ok|`OutOfBounds])
] with sexp
