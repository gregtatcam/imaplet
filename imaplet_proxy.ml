(*
 * Copyright (c) 2013-2014 Gregory Tsipenyuk <gt303@cam.ac.uk>
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
open Lwt
open ServerConfig

let try_close chan =
  catch (fun () -> Lwt_io.close chan)
  (function _ -> return ())

let try_close_sock sock =
  match sock with
  | Some sock -> catch (fun () -> Lwt_unix.close sock) (fun _ -> return ())
  | None -> return ()

let create_cert () = 
  X509_lwt.private_of_pems
  ~cert:"./certificates/server.pem"
  ~priv_key:"./certificates/server.key"

let init_socket addr port =
  Printf.printf "serverfe: creating socket %s %d\n%!" addr port;
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string addr, port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt_unix.bind socket sockaddr;
  socket
 
let create_srv_socket addr port =
  let socket = init_socket addr port in
  Lwt_unix.listen socket 10;
  socket

let accept_ssl sock cert =
  Tls_lwt.accept cert sock >>= fun (channels, addr) ->
  return (channels, addr, None)

let accept_cmn sock =
  Lwt_unix.accept sock >>= fun (sock_c, addr) ->
  let ic = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.input sock_c in
  let oc = Lwt_io.of_fd ~close:(fun()->return()) ~mode:Lwt_io.output sock_c in
  return ((ic,oc), addr, Some sock_c)

let accept_conn sock = function
  | Some cert -> accept_ssl sock cert
  | None -> accept_cmn sock

let start_server () =
  let init_ssl () =  
    Tls_lwt.rng_init () >>= fun () ->
    create_cert () in
  let init_ssl_ is_ssl =
    if is_ssl = true then (
      init_ssl() >>= fun cert ->
      return (Some cert)
    ) else (
      return (None)
    )
  in
  init_ssl_ srv_config.ssl >>= fun cert ->
  let srv_sock =create_srv_socket srv_config.addr srv_config.port in
  Printf.printf "imaplet_proxy started\n%!";
  (* accepts new connections, starts new thread for tranfsering data
   * between the imap server and the client, handles ssl/starttls
   *)
  let rec process_client_accept srv_sock is_ssl =
    Printf.printf "imaplet_proxy start accepting ssl:%b\n%!" is_ssl;
    catch (fun () ->
      accept_conn srv_sock cert 
    ) (fun ex ->
      match ex with
      | End_of_file -> accept_conn srv_sock cert
      | _ ->
        Printf.printf "imaplet_proxy accept exception %s\n%!" 
        (Core.Std.Exn.to_string ex); raise ex
    )
    >>= fun (channels, _, cl_sock) ->
    (* start new thread for connected client *)
    async ( fun () ->
      Printf.printf "imaplet_proxy accepted\n%!";
      let pause_ () = pause () in
      let wakeup_paused_ () = wakeup_paused () in
      let uni () = () in
      let uni_ret () = return () in
      let channels_of_sock sock = 
        (Lwt_io.of_fd
          ~close:(fun() -> return ()) ~mode:Lwt_io.input sock,
         Lwt_io.of_fd 
          ~close:(fun() -> return ()) ~mode:Lwt_io.output sock)
      in
      (* connect to the imap server *)
      let imap_sock = init_socket srv_config.addr 0 in
      let imapaddr = 
        Unix.ADDR_INET (Unix.inet_addr_of_string srv_config.imap_addr,
        srv_config.imap_port) in
      Lwt_unix.connect imap_sock imapaddr >>= fun () ->
      Printf.printf "imaplet_proxy connected to imap server\n%!";
      (* get imap upstream channels *)
      let (ic_up,oc_up) = channels_of_sock imap_sock in
      (* get client downstream channels *)
      let (ic,oc) = channels in
      let write_from_to is_ssl ps wk ic oc =
        ps () >>= fun () ->
        let buff = String.create 2048 in
        Lwt_io.read_into ic buff 0 2048 >>= fun i -> 
        let starttls = Core.Std.String.substr_index buff 
          ~pattern:"OK STARTTLS" <> None
        in
        if i = 0 then 
          return `Eof
        else (
          Lwt_io.write_from oc buff 0 i >>= fun _ -> 
          Lwt_io.flush oc >>= fun () -> 
          if starttls = true && is_ssl = false then
            return `Starttls
          else
            return `Ok
        ) >>= fun res -> wk (); return res
      in
      let rec write_from_to_loop is_ssl ps wk ic oc =
        write_from_to is_ssl ps wk ic oc >>= function
        | `Ok -> write_from_to_loop is_ssl uni_ret uni ic oc 
        | `Eof -> return `Eof
        | `Starttls -> return `Starttls
      in

      (* initial ssl (not starttls) has to be sequenced: read upstream ->
       * write downstream -> read downstream -> write upstream 
       * see ocaml-tls architecture (initial pull)
       * works for common socket too
       *)
      let rec loop cl_sock is_ssl ps wk ic_up oc_up ic oc =
        let open Core.Std in
        catch (fun() ->
          Lwt.pick [
           write_from_to_loop is_ssl uni_ret wk ic_up oc ;
           write_from_to_loop is_ssl ps uni ic oc_up ;
          ] >>= function
          | `Eof -> 
            Printf.printf "imaplet_proxy eof tls\n%!";
            return ()
          | `Starttls -> 
            Printf.printf "imaplet_proxy starting tls\n%!";
            init_ssl() >>= fun cert ->
            Printf.printf "imaplet_proxy got certificate\n%!";
            Tls_lwt.Unix.server_of_fd 
            (Tls.Config.server ~certificate:cert ()) 
            (Option.value_exn cl_sock) >>= fun srv ->
            Printf.printf "imaplet_proxy created ssl server\n%!";
            try_close ic >> try_close oc >>
            let ic,oc = Tls_lwt.of_t srv in 
              loop cl_sock true uni_ret uni ic_up oc_up ic oc
        ) 
        (fun ex ->
          Printf.printf "imaplet_proxy io exception %s\n%!" 
            (Exn.to_string ex); return ()
        ) >>= fun () ->
          Printf.printf "imaplet_proxy closing client/imap connection\n%!" ;
          try_close ic >> try_close oc >> 
          try_close ic_up >> try_close oc_up >>
          (if is_ssl then return () else try_close_sock cl_sock) >> 
          try_close_sock (Some imap_sock)
      in
      loop cl_sock is_ssl (pause_) (wakeup_paused_) ic_up oc_up ic oc
    ); 
    process_client_accept srv_sock is_ssl
  in
    process_client_accept srv_sock srv_config.ssl

let () =
  Lwt_main.run (catch(fun()->start_server ())
  (fun ex -> 
    Printf.printf "imaplet_proxy fatal exception %s %s\n%!" 
      (Core.Std.Exn.to_string ex) (Core.Std.Exn.backtrace());
    return()))
