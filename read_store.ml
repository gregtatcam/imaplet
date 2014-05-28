(*
 *
 *   Simple example showing how to create and use a Git store.
 *
 *     $ make                               # Compile
 *     $ ./git_store                        # Run
 *     $ cd /tmp/irmin/test && git log      # Show the Git history
 *
 *)


open Lwt
open IrminStorage
open Sexplib

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

let path = "/tmp/irmin/test"
module Git =
IrminGit.Make(IrminKey.SHA1)(IrminContents.String)(IrminReference.String)
module Store = (val Git.create ~bare:true ~kind:`Disk ~root:path ())

let in_line () =
  Lwt_io.read_line Lwt_io.stdin

let out_line str =
  Lwt_io.write Lwt_io.stdout str >>= fun () ->
  Lwt_io.flush Lwt_io.stdout

let prompt str =
  out_line str >>= fun () ->
  in_line () 

let rec selected user mbox =
  let open StorageMeta in
  let open Email_message in
  prompt (user ^ ":" ^ (IrminMailbox.to_string mbox) ^ ": ") >>= function 
  | "help" -> Printf.printf "all\nexists\nhelp\nlist\nmeta\nmessage\nclose\n%!";
  selected user mbox
  | "all" -> IrminMailbox.show_all mbox >>= fun () -> selected user mbox
  | "exists" -> IrminMailbox.exists mbox >>= fun res ->
    (
     match res with
    | `No -> Printf.printf "no\n%!"
    | `Folder -> Printf.printf "folder\n%!"
    | `Storage -> Printf.printf "storage\n%!"
    ); selected user mbox
  | "meta" -> IrminMailbox.get_mailbox_metadata mbox >>= fun meta ->
    Printf.printf "%s\n%!" (Sexp.to_string (sexp_of_mailbox_metadata meta));
    selected user mbox
  | "message" -> prompt "position? " >>= fun pos -> 
    (
    let pos = int_of_string pos in
    IrminMailbox.read_message mbox (`Position pos) >>= function
    | `Ok (message,meta) ->
      Printf.printf "%s\n%!" (Sexp.to_string (sexp_of_mailbox_message_metadata meta));
      Printf.printf "%s\n%!" (Sexp.to_string (Mailbox.Message.sexp_of_t message));
      return ()
    | `NotFound -> Printf.printf "not found\n%!"; return ()
    | `Eof -> Printf.printf "eof\n%!"; return ()
    ) >>= fun() -> selected user mbox
  | "list" -> 
    IrminMailbox.list_store mbox >>= fun l ->
    Core.Std.List.iter l ~f:(fun i ->
      match i with
      | `Folder (f,i) -> Printf.printf "folder/%d %s\n%!" i f;
      | `Storage s -> Printf.printf "storage %s\n%!" s;
    );
    selected user mbox
  | "close" -> return ()
  | _ -> Printf.printf "unknown command\n%!"; selected user mbox

let main () =
  Store.create () >>= fun t ->
  out_line "type help for commands\n" >>= fun () ->
  let rec request user =
    prompt (user ^ ": ") >>= function 
    | "help" -> Printf.printf "help\nselect\nlist\nuser\nquit\n%!"; request user
    | "user" -> prompt "user? " >>= fun user -> request user
    | "select" -> 
        prompt "mailbox? " >>= fun mailbox ->
          let mbox = IrminMailbox.create user mailbox in
          selected user mbox >>= fun () -> request user
    | "list" -> 
      let mbox = IrminMailbox.create user "" in
      IrminMailbox.list_store mbox >>= fun l ->
      Core.Std.List.iter l ~f:(fun i -> match i with
      | `Folder (i,c) -> Printf.printf "folder children:%d %s\n%!" c i
      | `Storage c -> Printf.printf "storage %s\n%!" c); request user
    | "quit" -> return ()
    | _ -> Printf.printf "unknown command\n%!"; request user
  in
  prompt "user? " >>= fun user ->
  request user 

let () =
  Lwt_unix.run (main())
