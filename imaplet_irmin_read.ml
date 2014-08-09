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
open Irmin_unix

exception InvalidCmd

let uinput = ref []

let arg n =
  if Core.Std.List.length !uinput > n then
    Core.Std.List.nth_exn !uinput n
  else
    raise InvalidCmd

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

let path = "/tmp/irmin/test"
(*
module Git =
IrminGit.Make(IrminKey.SHA1)(IrminContents.String)(IrminReference.String)
module Store = (val Git.create ~bare:true ~kind:`Disk ~root:path ())
*)

module Git = IrminGit.FS(struct
  let root = Some path
  let bare = true
end)

module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)

let in_line () =
  Lwt_io.read_line Lwt_io.stdin

let out_line str =
  Lwt_io.write Lwt_io.stdout str >>= fun () ->
  Lwt_io.flush Lwt_io.stdout

let prompt str =
  out_line str >>= fun () ->
  in_line () >>= fun msg ->
  uinput := (Core.Std.String.split msg ~on:' ');
  return (arg 0)

let rec selected user mbox =
  let open StorageMeta in
  let open Email_message in
  try
  prompt (user ^ ":" ^ (IrminMailbox.to_string mbox) ^ ": ") >>= function 
  | "help" -> Printf.printf "all\nexists\nhelp\nlist\nmeta\nmessage
  #\nclose\nremove uid\nexpunge\nstore # +-| flags-list%!";
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
  | "message" -> let pos = arg 1 in
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
  | "store" -> let pos = arg 1 in
  (
    let pos = int_of_string pos in
    IrminMailbox.read_message mbox (`Position pos) >>= function
    | `Ok (_,meta) ->
      let flags = Core.Std.List.foldi !uinput ~init:[] ~f:(
        fun i acc el -> Printf.printf "%s\n%!" el;if i < 3 then acc else
          (States.str_to_fl ("\\" ^ el)) :: acc) in
      let find l i = (Core.Std.List.find l ~f:(fun el -> if el = i then true else false)) <> None in
      let meta =
      (
      match (arg 2) with
      | "+" -> let flags = Core.Std.List.fold flags ~init:meta.flags ~f:(fun acc i -> 
            if find acc i then acc else i :: acc) in {meta with flags}
      | "-" -> let flags = Core.Std.List.fold meta.flags ~init:[] ~f:(fun acc i ->
          if find flags i then acc else i :: acc) in {meta with flags}
      | "|" -> {meta with flags}
      | _ -> raise InvalidCmd
      )
      in IrminMailbox.update_metadata mbox (`Position pos) meta >>= fun res ->
        ( match res with
        | `Ok -> Printf.printf "updated\n%!"
        | `Eof -> Printf.printf "eof\n%!"
        | `NotFound -> Printf.printf "not found\n%!"
        ); return ()
    | `NotFound -> Printf.printf "not found\n%!"; return ()
    | `Eof -> Printf.printf "eof\n%!"; return ()
  ) >>= fun () -> selected user mbox
  | "remove" -> let uid = arg 1 in IrminMailbox.remove mbox uid >>= fun () ->
      selected user mbox
  | "expunge" -> IrminMailbox.expunge mbox >>= fun deleted ->
      Core.Std.List.iter deleted ~f:(fun i -> Printf.printf "deleted %d\n%!" i);
      selected user mbox
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
  with InvalidCmd -> Printf.printf "unknown command\n%!"; selected user mbox

let main () =
  Store.create () >>= fun t ->
  out_line "type help for commands\n" >>= fun () ->
  let rec request user =
    try
    prompt (user ^ ": ") >>= function 
    | "help" -> Printf.printf "help\nselect mbox\nlist\nuser\nquit\n%!"; request user
    | "user" -> prompt "user? " >>= fun user -> request user
    | "select" -> 
          let mailbox = Str.replace_first (Str.regexp "+") " " (arg 1) in
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
    with InvalidCmd -> Printf.printf "unknown command\n%!"; request user
  in
  prompt "user? " >>= fun user ->
  request user 

let () =
  Lwt_unix.run (main())
