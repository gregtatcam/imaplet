open Core.Std
open Async.Std
open Async_unix


let spec =
let open Command.Spec in
empty
+> anon ("filename" %: string)

let mkfile file = file

let build name =
  let mbx = Amailbox.create "dovecot" in
  Amailbox.get_index_with_init ~with_mbox:true mbx (mkfile name) >>= fun _ -> return true

let concat a b =
  if a = "" then
    b
  else
    Filename.concat a b

let rec del_index_folders dir =
    Sys.ls_dir dir >>= fun l ->
      Deferred.List.fold l 
      ~init:false
      ~f:(fun _ item -> 
        printf "del_index_folders %s\n%!" item; 
        let file = mkfile (Filename.concat dir item) in
        Sys.is_directory_exn ~follow_symlinks:false file >>= fun isdir ->
        if isdir then (
          if Regex.match_regex item  "^.imaplet" then (
            printf "deleting index folder %s\n%!" file;
            Unix_syscalls.system_exn ("rm -rf " ^ (Regex.squote file)) >>= fun() -> return true
          )  else if item = ".imap" then (
            return true
          ) else (
            printf "a directory %s\n%!" file; del_index_folders file
          )
        ) else 
          return true
      )

let rec fold_dir dir relative =
    Sys.ls_dir dir >>= fun l ->
      Deferred.List.fold l 
      ~init:false
      ~f:(fun _ item -> 
        let file = mkfile (Filename.concat dir item) in
        Sys.is_directory_exn ~follow_symlinks:false file >>= fun isdir ->
        if isdir then (
          if item = ".imaplet" then (
            printf "deleting index folder %s\n%!" file;
            Unix_syscalls.system_exn ("rm -rf " ^ file) >>= fun() -> return true
          )  else if item = ".imap" then (
            return true
          )  else (
            printf "a directory %s\n%!" file; fold_dir file (concat relative item)
          )
        ) else if (String.nget item 0) = '.' then (
          printf "skiping special file %s\n%!" file;return true
        ) else (
          printf "building index for %s\n%!" file; 
          build (concat relative item)
        )
      )

let build_index (filename:string) : (bool Deferred.t) =
  del_index_folders filename >>= fun res ->
  fold_dir filename ""

let command =
    Command.basic
    ~summary:"print index file"
    spec
    (fun filename () -> 
      upon( build_index filename ) (fun res -> ());
    never_returns (Scheduler.go()))

let () = 
  try
    Command.run command
  with Exit ->
    Printexc.print_backtrace stderr
