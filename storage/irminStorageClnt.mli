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
open Async.Std
open Primitives
open Storage

module IrminsuleStorageAccessor : MailboxAccessor_intf with 
  type a = string*BasicLocation.t*Reader.t*Writer.t and
  type t = string*BasicLocation.t*Reader.t*Writer.t
 

module IrminsuleStorage : MailboxStorage_intf with 
  type loc = BasicLocation.t and 
  type param=string*Reader.t*Writer.t and 
  type t = string*BasicLocation.t*BasicLocation.t*BasicLocation.t*Reader.t*Writer.t and 
  type accs = IrminsuleStorageAccessor.t