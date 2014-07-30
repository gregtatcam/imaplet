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
(* ocamlfind c -w A -linkpkg -package lwt,lwt.unix,lwt.syntax -syntax
  camlp4o,lwt.syntax myecho.ml -o myecho *)
open Lwt
open IrminStorage
open Email_message
open Sexplib
open ServerConfig
 
let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())
 
let create_socket () =
  Printf.printf "irminStorageSrv: creating socket any %d\n%!" srv_config.irmin_port;
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, srv_config.irmin_port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  Lwt_unix.listen socket IrminStorageConfig.backlog;
  socket

let get_pos = function
  | `Position p -> "seq " ^ (string_of_int p)
  | `UID u -> "uid " ^ (string_of_int u)

let handle_reader user loc filter pos = 
  Printf.printf "------irminStorageSrv handle_reader %s %s %s\n%!" user loc (get_pos pos);
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.read_message mbox ?filter pos >>= function
  | `Ok (msg, meta) ->
      return (`Reader (`Ok (msg, meta)))
  | `NotFound -> return (`Reader `NotFound)
  | `Eof -> return (`Reader `Eof)

let handle_reader_metadata user loc filter pos =
  Printf.printf "------irminStorageSrv handle_reader_metadata %s %s %s\n%!" user loc (get_pos pos);
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.read_metadata mbox ?filter pos >>= function
  | `Ok meta -> return (`Reader_metadata (`Ok meta))
  | `NotFound -> return (`Reader_metadata `NotFound)
  | `Eof -> return (`Reader_metadata `Eof)

let handle_writer user loc message metadata =
  Printf.printf "------irminStorageSrv handle_writer %s %s \n%!" user loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.add mbox message metadata >>= fun () ->
    return (`Writer `Ok)

let handle_writer_metadata user loc pos metadata =
  Printf.printf "------irminStorageSrv handle_writer_metadata %s %s %s\n%!" user loc (get_pos pos);
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.update_metadata mbox pos metadata >>= function
    | `Ok -> return (`Writer_metadata `Ok)
    | `NotFound -> return (`Writer_metadata `NotFound)
    | `Eof -> return (`Writer_metadata `Eof)

let handle_exists user loc =
  Printf.printf "------irminStorageSrv handle_exists %s %s\n%!" user loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.exists mbox >>= fun res ->
    return (`Exists res)

let handle_create user loc = 
  Printf.printf "------irminStorageSrv handle_create mailbox %s %s\n%!" user loc;
  let open Core.Std in
  let mbox = IrminMailbox.create user loc in
  let (folders,created) = 
    if String.nget loc ((String.length loc) - 1) = '/' then
      true,`Folder
    else
      false,`Storage
  in
  IrminMailbox.create_mailbox mbox ~folders >>= fun () ->
    return (`Create created)

let handle_move user loc1 loc2 =
  Printf.printf "------irminStorageSrv handle_move %s %s %s\n%!" user loc1 loc2;
  let mbox1 = IrminMailbox.create user loc1 in
  let mbox2 = IrminMailbox.create user loc2 in
  IrminMailbox.move_mailbox mbox1 mbox2 >>= fun () ->
    return `Move

let handle_delete user loc =
  Printf.printf "------irminStorageSrv handle_delete %s %s\n%!" user loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.delete_mailbox mbox >>= fun () ->
    return `Delete

let handle_expunge user loc =
  Printf.printf "------irminStorageSrv handle_expunge %s %s\n%!" user loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.expunge mbox >>= fun res ->
    Printf.printf "%d records expunged\n%!" (Core.Std.List.length res);
    return (`Expunge res)

let handle_copy user loc1 loc2 filter =
  Printf.printf "------irminStorageSrv handle_copy %s %s %s\n%!" user loc1 loc2;
  let mbox1 = IrminMailbox.create user loc1 in
  let mbox2 = IrminMailbox.create user loc2 in
  IrminMailbox.copy_mailbox mbox1 mbox2 filter >>= fun res ->
    return (`Copy_with res)

let handle_update_index user loc = return (`Update_index `Ok)

let handle_rebuild_index user loc = return `Rebuild_index

let handle_get_metadata user loc =
  Printf.printf "------irminStorageSrv handle_get_metadata %s %s\n%!" user loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.get_mailbox_metadata mbox >>= fun res ->
  return (`Mailbox_metadata res)

let handle_get_subscription user =
  Printf.printf "------irminStorageSrv handle_get_subscription %s\n%!" user;
  let mbox = IrminMailbox.create user "" in
  IrminMailbox.get_subscription mbox >>= fun res ->
  return (`Get_subscription res)

let handle_subscribe user mailbox =
  Printf.printf "------irminStorageSrv handle_subscribe %s %s\n%!" user mailbox; 
  let mbox = IrminMailbox.create user "" in
  IrminMailbox.subscribe mbox mailbox >>= fun () ->
  return (`Subscribe )

let handle_unsubscribe user mailbox =
  Printf.printf "------irminStorageSrv handle_unsubscribe %s %s\n%!" user mailbox;
  let mbox = IrminMailbox.create user "" in
  IrminMailbox.unsubscribe mbox mailbox >>= fun () ->
  return (`Unsubscribe )

let handle_list_store user loc =
  Printf.printf "------irminStorageSrv handle_list_store %s %s\n%!" user loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.list_store mbox >>= fun res ->
    return (`List_store res)

let handle_search_with user loc filter =
  Printf.printf "------irminStorageSrv handle_search_with %s\n%!" loc;
  let mbox = IrminMailbox.create user loc in
  IrminMailbox.search_with mbox filter >>= fun res ->
    return (`Search_with res)

let handle_create_account user =
  Printf.printf "------irminStorageSrv handle_create_account %s\n%!" user;
  let mbox = IrminMailbox.create user "" in
  IrminMailbox.create_account mbox >>= fun res ->
    return (`Create_account res)

let handle_remove_account user =
  Printf.printf "------irminStorageSrv handle_remove_account %s\n%!" user;
  let mbox = IrminMailbox.create user "" in
  IrminMailbox.remove_account mbox >>= fun res ->
    return (`Remove_account res)

let process_request outchan msg = 
  let open StorageMeta in
  let open Email_message in
  let open IrminStorageCmd in
  let msg = irminRequest_of_sexp (Sexp.of_string msg) in
  let write = Lwt_io.write_value outchan in
  (match msg with
  | `Bootstrap -> return `Bootstrap
  | `Copy_with (user, loc1, loc2, filter) -> handle_copy user loc1 loc2 filter
  | `Create (user,loc) -> handle_create user loc
  | `Create_account (user) -> handle_create_account user 
  | `Delete (user, loc) -> handle_delete user loc
  | `Exists (user, loc) -> handle_exists user loc
  | `Expunge (user, loc) -> handle_expunge user loc
  | `Get_subscription user -> handle_get_subscription user
  | `List_store (user, loc) -> handle_list_store user loc
  | `Mailbox_metadata (user, loc) -> handle_get_metadata user loc
  | `Move (user, loc1, loc2) -> handle_move user loc1 loc2
  | `Reader (user,loc,pos,filter) -> handle_reader user loc filter pos 
  | `Reader_metadata (user,loc,filter,pos) -> handle_reader_metadata user loc filter pos
  | `Remove_account (user) -> handle_remove_account user 
  | `Rebuild_index (user, loc) -> handle_rebuild_index user loc
  | `Search_with (user, loc, filter) -> handle_search_with user loc filter
  | `Subscribe (user, mbox) ->handle_subscribe user mbox
  | `Writer (user,loc,append, (message,metadata)) -> handle_writer user loc message metadata
  | `Writer_metadata (user, loc, pos, metadata) -> handle_writer_metadata user loc pos metadata
  | `Update_index (user, loc) ->handle_update_index user loc
  | `Unsubscribe (user,mbox) -> handle_unsubscribe user mbox
  ) >>= fun response -> 
    let sexp = sexp_of_irminResponse response in
    let buff = Sexp.to_string sexp in
    write buff >>= fun () -> Lwt_io.flush outchan


let rec requests inchan outchan =
  catch (fun () -> 
    Lwt_io.read_value inchan >>= fun msg -> Printf.printf "irminStorageSrv requests\n%!";
    process_request outchan msg >>= fun () -> requests inchan outchan
  )
  (fun ex -> 
    (match ex with 
    | End_of_file ->
      Printf.printf "irminStorageSrv: connection closed\n%!";
    | _ ->
      Printf.printf "%s\n%!" (Core.Exn.to_string ex);
    );
    try_close inchan >> try_close outchan >> return() 
  )
 
let process socket =
  Printf.printf "irminStorageSrv processing socket\n%!";
  let rec _process () =
    Lwt_unix.accept socket >>=
      (fun (socket_cli, _) ->
        Printf.printf "irminStorageSrv accepted socket\n%!";
        let inchan = Lwt_io.of_fd ~mode:Lwt_io.input socket_cli in
        let outchan = Lwt_io.of_fd ~mode:Lwt_io.output socket_cli in
        async (fun () -> requests inchan outchan);
        _process ()
      )
  in
  _process ()
 
let _ =
  Printf.printf "irminStorageSrv started\n%!";
  let socket = create_socket () in
  Lwt_main.run (
    process socket
  )
