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
open Email_message
open Email_message.Mailbox.Message
open Storage
open ServerConfig

let spec =
let open Command.Spec in
empty
  +> flag "-u" (required string) ~doc:"user"

let marshal a =
    Marshal.to_string a [Marshal.Compat_32]

let init_irmin_storage () =
  let open IrminStorageCmd in
    Unix_syscalls.fork_exec
    ~prog:(Configuration.irmin_srv_exec)
    ~args:[Configuration.irmin_srv_exec] ()
    >>= fun pid -> 
    printf "launched irmin server pid %d\n%!" (Pid.to_int pid);
    try_with (fun () ->
      after (Core.Span.create ~sec:1 ()) >>= fun () -> 
      printf "started irminStorage\n%!";
      let where = Tcp.to_host_and_port (srv_config.irmin_addr)
      (srv_config.irmin_port) in
      Tcp.connect where >>= fun (_,r,w) -> 
      Writer.write w (marshal (Sexp.to_string (sexp_of_irminRequest `Bootstrap))) ; 
      Writer.flushed w >>= fun () ->
      Reader.read_marshal r >>= function 
       | `Ok _ -> printf "boostrapped\n%!"; return (Some (r,w))
       | `Eof -> printf "failed to bootstrap the store: eof\n%!"; return None
    ) >>= function
      | Ok res -> return (pid,res)
      | Error ex -> printf "failed to boostrap the store: exception %s\n"
      (Exn.to_string ex); return (pid,None)

let unix_mbox_mailbox loc mbox_root inbox_root =
  let open Storage in
  build_strg_inst (module UnixMboxMailboxStorage) (loc, mbox_root, inbox_root,
  Configuration.mbox_index_params) ()

let irmin_mailbox user loc mbox_root inbox_root rw =
  let open Storage in
  let open IrminStorageClnt in
  let (r,w) = rw in
  let param = (user,r,w) in
  build_strg_inst (module IrminsuleStorage) (loc, mbox_root, inbox_root, param) ()

let get_storage user str_rw file mbox_root inbox_root =
  let open Primitives in
  let loc = BasicLocation.create file in
  match str_rw with 
  | None -> unix_mbox_mailbox loc mbox_root inbox_root
  | Some rw -> irmin_mailbox user loc mbox_root inbox_root rw

(**
 * handle command line
**)
let command =
  let open Primitives in
  let folder f = 
    "/" ^ f ^ "/"
  in

  let storage s =
    "/" ^ s
  in

  let print_created = function
    | `Folder -> printf "created folder\n%!"
    | `Storage -> printf "created storage\n%!"
  in

  let if_template (message:Mailbox.Message.t) =
    let open Regex in
    let header = Header.to_list (Email.header message.email) in
    List.find header ~f:(fun (n,v) -> 
      if match_regex n "From" && match_regex v "Mail System Internal Data" then
        true
      else
        false
    ) <> None
  in

  let create_mailbox user str_rw file_mailbox irmin_mailbox =
    let open StorageMeta in
    let (module IrminMailbox) = get_storage user str_rw irmin_mailbox "/" "/" in
    IrminMailbox.MailboxStorage.create IrminMailbox.this >>= fun s -> 
      Mailbox.With_pipe.t_of_file file_mailbox >>= fun mailbox ->
      IrminMailbox.MailboxStorage.fold IrminMailbox.this ~exclusive:true ~init:() ~f:(fun () accs ->
        try_with (fun() ->
          let (module Accessor : StorageAccessor_inst) = accs in
          Pipe.fold mailbox 
          ~init:()
          ~f:(fun () message -> 
            if if_template message then
              return ()
            else
              let meta = empty_mailbox_message_metadata() in
              let meta = {meta with flags = [Mflags.Flags_Seen]} in
              Accessor.StorageAccessor.writer Accessor.this `Append
              (message,meta) >>= function
              | `Ok -> printf "added message to the mailbox %s\n%!" irmin_mailbox; return ()
          )
        ) >>= fun _ -> return ()
      )
  in

  Command.async_basic
    ~summary:"run imaplet server"
    spec
      (fun user () -> 
          init_irmin_storage () >>= fun (pid,str_rw) ->
          let (module IrminMailbox) = get_storage user str_rw "" "/" "/" in
          IrminMailbox.MailboxStorage.remove_account IrminMailbox.this >>= fun _ ->
          IrminMailbox.MailboxStorage.create_account IrminMailbox.this >>= function
          | `Exists -> assert(false)
          | `Ok ->
          create_mailbox user str_rw (Configuration.inbox user) (storage "INBOX") >>= fun () ->
          let (module Mailbox) = get_storage user None
                    (Configuration.mailboxes user) (Configuration.mailboxes user)
                    (Configuration.inbox_root) in
          Mailbox.MailboxStorage.list_store Mailbox.this 
          ~init:()
          ~f:(fun () item ->
            try_with (fun() ->
              match item with
            | `Folder (item,cnt) -> printf "folder %s\n%!" item; 
              let (module IrminMailbox) = get_storage user str_rw (folder item) "/" "/" in
              IrminMailbox.MailboxStorage.create IrminMailbox.this >>= fun s -> 
                print_created s; return ()
            | `Storage item ->
              if item = ".subscriptions" then
                return()
              else (
                printf "storage %s\n%!" item; 
                create_mailbox user str_rw ((Configuration.mailboxes user) ^ (storage item)) (storage item)
              )
            ) >>= function
            | Ok () -> return ()
            | Error ex -> printf "%s\n%!" (Exn.to_string ex); return ()
          ) >>= fun () -> Unix_syscalls.system_exn ("kill -9 " ^ (Pid.to_string pid))
      )

(**
 * start the server
**)
let () = 
  try
    Command.run command
  with Exit ->
    Printexc.print_backtrace stderr
