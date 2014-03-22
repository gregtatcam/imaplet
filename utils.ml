open Core.Std
open Async.Std
open Async_unix

let sp() = " "

let star() = "*"

let crlf() = "\r\n"

let with_crlf text = text ^ crlf()

let to_qstr str = ("\"" ^ str ^ "\"")

let to_qnv name value = to_qstr name ^ sp() ^ to_qstr value

let to_plist l = "(" ^ l ^ ")"

let to_blist l = "[" ^ l ^ "]"

let substr str ~start ~size =
  let len = String.length str in
  let str =
  if len > start then
    Str.string_after str start
  else
    str
  in
  match size with
  | None -> str
  | Some size ->
    let len = String.length str in
    if len > size then
      Str.string_before str size
    else
      str

let send w buff =
  Writer.write w buff ~len:(String.length(buff));
  upon (Writer.flushed w) (fun() ->())

let send_wcrlf w buff =
  let resp = buff ^ crlf() in
  Writer.write w resp ~len:(String.length(resp));
  upon (Writer.flushed w) (fun() ->())

let formated_capability capability =
  to_blist("CAPABILITY " ^ capability)

let formated_id id =
  "ID " ^ to_plist (id)

let parse_users buff user password =
  try 
   let _ = Str.search_forward (Str.regexp
   "^\\([^:]+\\):{\\([^}]+\\)}\\([^:]+\\):") buff 0 in
   let u = Str.matched_group 1 buff in
   let p = Str.matched_group 3 buff in
   let t = Str.matched_group 2 buff in
   if u = user && p = password && t = "PLAIN" then
    true
   else
    false
  with _ ->
    false

let parse_user_b64 b64 =
  try 
   let buff = Base64.str_decode b64 in (** need to log this if it fails **)
   let _ = Str.search_forward (Str.regexp
   "^\\([^\\]+\\)\000\\([^\\]+\\)\000\\([^\\]+\\)$") buff 0 in
   let u1 = Str.matched_group 1 buff in
   let u2 = Str.matched_group 2 buff in
   let p = Str.matched_group 3 buff in
   if u1 = u2 then
     Some (u1,p)
   else
     None
  with _ ->
    None

let rec read_users r user password =
  Reader.read_line r >>= 
    function 
      | `Ok res -> 
        if parse_users res user password then
          return (true)
        else
          read_users r user password
      | `Eof -> return (false)

(** have to make users configurable **)
let authenticate_user ?(users="./users") user password =
  Reader.with_file users ~f:(fun r -> read_users r user password)

(** read into the Buffer the size specified by {#} **)
let rec read_literal (reader:Reader.t) (size:int) (cb:([`Ok|`Eof] -> string -> unit)) 
  : [`Ok|`Eof of int] Deferred.t =
  printf "read_literal %d\n%!" size;
  let len = (if size <= 10*1024 then size else 10*1024) in
  let buff = String.create len in
  Reader.really_read reader ~pos:0 ~len buff >>= function
    | `Ok -> cb `Ok buff;
    let size = size - len in
    if size > 0 then
      read_literal reader size cb
    else
      return (`Ok)
    | `Eof i -> 
      cb `Eof (Str.first_chars buff i);
      if i = size then (** completed required size **)
        return (`Ok)
      else
        return (`Eof (size-i))

(** create folder **)
let create_folder ~perm folder =
  try_with (fun() ->
    Unix_syscalls.mkdir ~p:() ~perm folder
  ) >>= function 
    | Ok () -> return (true)
    | Error _ -> return (false)

(** delete an empty folder **)
let delete_folder folder =
  try_with (fun() -> 
    Unix.rmdir folder
  ) >>= function
    | Ok() -> return (true)
    | Error _ -> return (false)

(** create directory with parents option **)
let mkdir ~perm ?(parent=false) folder =
  try_with (fun() ->
    Unix_syscalls.mkdir ~p:() ~perm folder
  ) >>= function 
    | Error _ -> return (false)
    | Ok _ -> 
    try_with ( fun() ->
      Unix.chmod ~perm folder
    ) >>= function
      | Error _ -> delete_folder folder >>= fun _ -> return (false)
      | Ok () -> return (true) 

(** delete a file **)
let delete_file file =
  try_with (fun() -> 
    Sys.remove file
  ) >>= function
    | Ok() -> return (true)
    | Error _ -> return (false)

(** rename a file/folder **)
let rename src dst =
  try_with (fun() ->
    Sys.rename src dst
  ) >>= function
    | Ok() -> return (true)
    | Error _ -> return (false)

(** check if file exists **)
let file_exists file =
  printf "file_exists %s\n%!" file;
  try
    Sys.file_exists_exn file >>= fun e -> return (e)
  with _ -> return (false)

(** check if file is directory **)
let is_directory file =
  try
    Sys.is_directory_exn ~follow_symlinks:false file >>= fun e -> return (e)
  with _ -> return (false)

let concat_path a1 a2 =
  if a1 <> "" && a2 <> "" then
    Filename.concat a1 a2
  else if a1 <> "" then
    a1
  else 
    a2

(** build path from individual componente without duplicating / **)
let make_path l =
  List.fold l ~init:"" 
  ~f:(fun acc item -> 
    if acc = "" then
      item
    else if item = "" then
      acc
    else 
      concat_path acc item
  )
