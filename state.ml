open Core.Std
open Async.Std
open Parsing
open Lexing
open Lex
open States
open Response
open Utils
open Contextlist
open Contexts
open Regex

exception SystemError of string

let make_exec_ctx req_ctx state_ctx mbx_ctx =
  {req_ctx; state_ctx; mbx_ctx};;

let make_resp_ctx resp_state_ctx resp_ctx resp_mbx_ctx =
  {resp_state_ctx; resp_ctx; resp_mbx_ctx};;

let return_resp_ctx resp_state_ctx resp_ctx resp_mbx_ctx =
  return (make_resp_ctx resp_state_ctx resp_ctx resp_mbx_ctx);;

(** parse the buffer, return ok (request) or error (msg) **)
let get_request_context contexts buff =
  printf "get_request_context %s\n%!" buff; Out_channel.flush stdout;
  first := true;
  try
    let lexbuff = Lexing.from_string buff in
    let command = (Parser.request Lex.read lexbuff) in
    printf "get_request_context, returned from parser\n%!"; Out_channel.flush stdout;
    let t =
    (
      if is_done command then
        let idle = find_idle contexts.req_ctx in
            (match idle with Some idle -> idle.request.tag | None -> "")
       else
          command.tag
    ) in
    Ok ({request={command with tag = t}})
  with
  | SyntaxError e -> printf "get_request_context error %s\n%!" e; Error (e)
  | Parser.Error -> printf "get_request_context bad command\n%!"; Error ("bad command")
  | Interpreter.InvalidSequence -> Error ("bad command")
  | Regex.InvalidDate -> Error("bad command")
  | e -> Error(Exn.backtrace())

(** handle all commands
 * return either Ok, Bad, No, Preauth, Bye, or Continue response
**)
(**
 * Any state
**)
let handle_id writer l =
  write_resp writer (Resp_Untagged (formated_id(Configuration.id())));
  return_resp_ctx None (Resp_Ok (None, "ID completed")) None

let handle_capability writer = 
  write_resp writer (Resp_Untagged (formated_capability(Configuration.capability())));
  return_resp_ctx None (Resp_Ok (None, "CAPABILITY completed")) None

let handle_logout writer =
  write_resp writer (Resp_Bye(None,""));
  return_resp_ctx (Some State_Logout) (Resp_Ok (None, "LOGOUT completed")) None

(** TBD should have a hook into the maintenance to recet inactivity **)
let handle_noop () =
  return_resp_ctx None (Resp_Ok (None, "NOOP completed")) None

(** TBD, needs work, server should send updates while idle is active (terminated
 * by clien'ts done)
**)
let handle_idle writer =
  return_resp_ctx None (Resp_Any ("+ idling")) None

let handle_done writer =
  return_resp_ctx None (Resp_Ok (None, "IDLE")) None

(**
 * Not Authenticated state
**)
let handle_authenticate writer auth_type text =
  Account.authenticate writer auth_type text >>= function
  | Ok (m,u) -> return_resp_ctx (Some State_Authenticated) m (Some (Amailbox.create u))
  | Error e -> return_resp_ctx None e None

let handle_login writer user password =
  Account.login writer user password >>= function
  | Ok (m,u) -> return_resp_ctx (Some State_Authenticated) m (Some (Amailbox.create u))
  | Error e -> return_resp_ctx None e None
(**
 * Done Not Authenticated state
**)

(**
 * Authenticated state
**)

let quote_file file =
  if match_regex file "[ ]" then
    "\"" ^ file ^ "\""
  else
    file

let list_resp flags file =
  let flags_str = String.concat ~sep:" " flags in
  let l = List.concat [["LIST ("]; [flags_str]; [") \"/\" "]; [quote_file file]] in 
  Resp_Untagged(String.concat ~sep:"" l)

let handle_list writer reference mailbox contexts lsub =
  (if lsub = false then
    Amailbox.listmbx contexts.mbx_ctx reference mailbox
  else
    Amailbox.lsubmbx contexts.mbx_ctx reference mailbox
  )>>= fun l ->
    List.iter l 
    ~f:(fun (file, flags) ->
      write_resp writer (list_resp flags file)
    );
  return_resp_ctx None (Resp_Ok(None, "LIST completed")) None

(** review - where the flags are coming from TBD **)
let handle_select writer mailbox contexts rw =
  let open Amailbox in
  (if rw then
    select contexts.mbx_ctx mailbox
  else
    examine contexts.mbx_ctx mailbox
  ) >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist:" ^ mailbox)) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable :" ^ mailbox)) None
  | `Error e -> return_resp_ctx None (Resp_No(None, e)) None
  | `Ok (mbx, header) ->
    if header.uidvalidity = "" then (** give up TBD **)
    (
      return_resp_ctx None (Resp_No(None,"Uidvalidity failed")) None
    )
    else
    (
      let (flags,prmnt_flags) = Configuration.get_mbox_flags in
      let flags = to_plist (String.concat ~sep:" " flags) in
      write_resp writer (Resp_Untagged ("FLAGS " ^ flags));
      let flags = to_plist (String.concat ~sep:" " prmnt_flags) in
      write_resp writer (Resp_Ok (Some RespCode_Permanentflags, flags));
      write_resp writer (Resp_Untagged ((string_of_int header.count) ^ " EXISTS"));
      write_resp writer (Resp_Untagged ((string_of_int header.recent) ^ " RECENT"));
      write_resp writer (Resp_Ok (Some RespCode_Uidvalidity, header.uidvalidity));
      write_resp writer (Resp_Ok (Some RespCode_Uidnext, string_of_int header.uidnext));
      if rw then
        return_resp_ctx (Some State_Selected) (Resp_Ok(Some RespCode_Read_write, "")) (Some mbx)
      else
        return_resp_ctx (Some State_Selected) (Resp_Ok(Some RespCode_Read_only, "")) (Some mbx)
    )

(** create a mailbox **)
let handle_create writer mailbox contexts =
  Amailbox.create_mailbox contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "CREATE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** delete a mailbox **)
let handle_delete writer mailbox contexts =
  Amailbox.delete_mailbox contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "DELETE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** rename a mailbox **)
let handle_rename writer src dest contexts = 
  Amailbox.rename_mailbox contexts.mbx_ctx src dest >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "RENAME completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** subscribe a mailbox **)
let handle_subscribe writer mailbox contexts = 
  Amailbox.subscribe contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "SUBSCRIBE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

(** subscribe a mailbox **)
let handle_unsubscribe writer mailbox contexts = 
  Amailbox.unsubscribe contexts.mbx_ctx mailbox >>= function
    | `Ok -> return_resp_ctx None (Resp_Ok(None, "UNSUBSCRIBE completed")) None
    | `Error e -> return_resp_ctx None (Resp_No(None,e)) None

let handle_status writer mailbox optlist contexts =
  let open Amailbox in
    examine contexts.mbx_ctx mailbox >>= function
    | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist:" ^ mailbox)) None
    | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable :" ^ mailbox)) None
    | `Error e -> return_resp_ctx None (Resp_No(None, e)) None
    | `Ok (mbx, header) ->
    if header.uidvalidity = "" then (** give up TBD **)
    (
      return_resp_ctx None (Resp_No(None,"Uidvalidity failed")) None
    )
    else
    (
      let output = (List.fold optlist ~init:"" ~f:(fun acc opt ->
        let str = (match opt with
        | Stat_Messages -> "EXISTS " ^ (string_of_int header.count)
        | Stat_Recent -> "RECENT " ^ (string_of_int header.recent)
        | Stat_Uidnext -> "UIDNEXT " ^(string_of_int header.uidnext)
        | Stat_Uidvalidity -> "UIDVALIDITY " ^ header.uidvalidity
        | Stat_Unseen -> "UNSEEN " ^ (string_of_int header.nunseen)
        ) in 
        if acc = "" then
          acc ^ str
        else
          acc ^ " " ^ str
      )) in
      write_resp writer (Resp_Untagged (to_plist output));
      return_resp_ctx None (Resp_Ok(None, "STATUS completed")) None
    )

(** handle append **)
let handle_append reader writer mailbox flags date literal contexts =
  printf "handle_append\n%!";
  let open Amailbox in
  (** is the size sane? **)
  let size = (match literal with
  | Literal n -> n
  | LiteralPlus n -> n) in
  if size > Configuration.max_message_size then
    return_resp_ctx None (Resp_No(None,"Max message size")) None
  else (
    append contexts.mbx_ctx mailbox reader writer flags date literal >>= function
      | `NotExists -> return_resp_ctx None (Resp_No(Some RespCode_Trycreate,"")) None
      | `NotSelectable -> return_resp_ctx None (Resp_No(Some RespCode_Trycreate,"Noselect")) None
      | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
      | `Eof i -> return_resp_ctx (Some State_Logout) (Resp_No(None, "Truncated Message")) None
      | `Ok header -> return_resp_ctx None (Resp_Ok(None, "APPEND completed")) None
  )
(**
 * Done Authenticated state
**)

(**
 * Selected state
**)

let handle_close writer contexts context =
  let mbx = Amailbox.close contexts.mbx_ctx in
  return_resp_ctx (Some State_Authenticated) (Resp_Ok(None, "CLOSE completed")) (Some mbx)

let rec print_search_tree t indent =
  printf "search ------\n%!";
  let indent = indent ^ " " in
  let open Amailbox in
  match t with
  | Key k -> printf "%s-key\n%!" indent
  | KeyList k -> printf "%s-key list %d\n%!" indent (List.length k);List.iter k ~f:(fun i -> print_search_tree i indent)
  | NotKey k -> printf "%s-key not\n%!" indent; print_search_tree k indent
  | OrKey (k1,k2) -> printf "%s-key or\n%!" indent; print_search_tree k1 indent; print_search_tree k2 indent

(** handle the charset TBD **)
let handle_search writer charset search buid context =
  Amailbox.search context.mbx_ctx search buid >>= function 
    (** what do these two states mean in this contex? TBD **)
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok r -> 
      write_resp writer (Resp_Untagged (List.fold r ~init:""  ~f:(fun acc i ->
        let s = string_of_int i in
        if acc = "" then 
          s 
        else 
          s ^ " " ^ acc)
      ));
      return_resp_ctx None (Resp_Ok(None, "SEARCH completed")) None

let handle_fetch writer sequence fetchattr buid context =
  printf "handle_fetch\n";
  Amailbox.fetch context.mbx_ctx (write_resp_untagged writer) sequence fetchattr buid >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok () -> return_resp_ctx None (Resp_Ok(None, "FETCH completed")) None

let handle_store writer sequence flagsatt flagsval buid contexts =
  printf "handle_store %d %d\n" (List.length sequence) (List.length flagsval);
  Amailbox.store contexts.mbx_ctx (write_resp_untagged writer) sequence flagsatt flagsval buid >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok () -> return_resp_ctx None (Resp_Ok(None, "STORE completed")) None

let handle_copy writer sequence mailbox buid contexts =
  printf "handle_copy %d %s\n" (List.length sequence) mailbox;
  Amailbox.copy contexts.mbx_ctx mailbox sequence buid >>= function
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok () -> return_resp_ctx None (Resp_Ok(None, "COPY completed")) None

let handle_expunge writer contexts =
  printf "handle_expunge\n";
  Amailbox.expunge contexts.mbx_ctx (write_resp_untagged writer) >>= function
  (**
  | `NotExists -> return_resp_ctx None (Resp_No(None,"Mailbox doesn't exist")) None
  | `NotSelectable ->  return_resp_ctx None (Resp_No(None,"Mailbox is not selectable")) None
  **)
  | `Error e -> return_resp_ctx None (Resp_No(None,e)) None
  | `Ok -> return_resp_ctx None (Resp_Ok(None, "EXPUNGE completed")) None

(**
 * Done Selected state
**)

let handle_any writer request context = match request with
  | Cmd_Id l -> handle_id writer l
  | Cmd_Capability -> handle_capability writer
  | Cmd_Noop -> handle_noop()
  | Cmd_Logout -> handle_logout writer

let handle_notauthenticated writer request context = match request with
  | Cmd_Authenticate (a,s) -> handle_authenticate writer a s
  | Cmd_Login (u, p) -> handle_login writer u p
  | Cmd_Starttls -> return_resp_ctx None (Resp_Bad(None,"")) None

let handle_authenticated reader writer request contexts context = match request with
  | Cmd_Select mailbox -> handle_select writer mailbox contexts true
  | Cmd_Examine mailbox -> handle_select writer mailbox contexts false
  | Cmd_Create mailbox -> handle_create writer mailbox contexts
  | Cmd_Delete mailbox -> handle_delete writer mailbox contexts
  | Cmd_Rename (mailbox,to_mailbox) -> handle_rename writer mailbox to_mailbox contexts
  | Cmd_Subscribe mailbox -> handle_subscribe writer mailbox contexts
  | Cmd_Unsubscribe mailbox -> handle_unsubscribe writer mailbox contexts
  | Cmd_List (reference, mailbox) -> handle_list writer reference mailbox contexts false
  | Cmd_Lsub (reference, mailbox) -> handle_list writer reference mailbox contexts true
  | Cmd_Status (mailbox,optlist) -> handle_status writer mailbox optlist contexts
  | Cmd_Append (mailbox,flags,date,size) -> handle_append reader writer mailbox flags date size contexts
  | Cmd_Idle -> handle_idle writer
  | Cmd_Done -> handle_done writer

let handle_selected writer request contexts context = match request with
  | Cmd_Check -> return_resp_ctx None (Resp_Bad(None,"")) None
  | Cmd_Close -> handle_close writer contexts context
  | Cmd_Expunge -> handle_expunge writer contexts
  | Cmd_Search (charset,search, buid) -> handle_search writer charset search buid contexts
  | Cmd_Fetch (sequence,fetchattr, buid) -> handle_fetch writer sequence fetchattr buid contexts
  | Cmd_Store (sequence,flagsatt,flagsval, buid) -> handle_store writer sequence flagsatt flagsval buid contexts
  | Cmd_Copy (sequence,mailbox, buid) -> handle_copy writer sequence mailbox buid contexts

let handle_commands reader writer contexts context = 
  try_with ( fun () -> 
    let state = contexts.state_ctx in
    (
      match context.request.req with
      | Any r -> printf "handling any\n%!"; handle_any writer r context
      | Notauthenticated r when state = State_Notauthenticated-> 
        printf "handling nonauthenticated\n%!"; handle_notauthenticated writer r context
      | Authenticated r when state = State_Authenticated || state = State_Selected -> 
        printf "handling authenticated\n%!"; handle_authenticated reader writer r contexts context
      | Selected r when state = State_Selected -> 
        printf "handling selected\n%!"; handle_selected writer r contexts context
      | Done -> printf "Done, should log out\n%!"; 
        return_resp_ctx (Some State_Logout) (Resp_Bad(None,"")) None
      | _ -> return_resp_ctx None (Resp_Bad(None, "Bad Command")) None
    ) >>= fun rsp_ctx ->
    match rsp_ctx.resp_state_ctx with
    |Some state -> return ({rsp_ctx with resp_state_ctx = Some state})
    |None -> return ({rsp_ctx with resp_state_ctx = Some state})
  ) >>= function
    | Ok res -> return res
    | Error ex -> printf "%s\n%!" (Exn.to_string ex);
        return_resp_ctx (Some contexts.state_ctx) (Resp_Bad(None, "Bad Command")) None (** need to handle this TBD **)

(** need to add tag to the response as needed **)
let handle_response w context response =
  printf "handle_response\n%!";
  match context with 
  | Ok context -> write_resp w ~tag:context.request.tag response
  | Error e -> write_resp w response

let pr_state contexts = match contexts.state_ctx with
  |State_Notauthenticated -> printf "in notauthenticated state\n%!"
  |State_Authenticated -> printf "in authenticated state\n%!"
  |State_Selected -> printf "in selected state\n%!"
  |State_Logout -> printf "in logout state\n%!"

let update_contexts contexts context response =
  printf "update_contexts %d\n%!" (length contexts.req_ctx);
  let _ = pop contexts.req_ctx in 
  match context with 
  |Ok (ctx) -> pushs contexts.req_ctx ctx
  |Error (e) -> contexts.req_ctx


let rec read_network reader writer buffer =
  printf "read_network\n%!";
  Reader.read_line reader >>=
    function
      | `Eof -> return (`Eof)
      | `Ok buff ->
          let i = match_regex_i buff "{\\([0-9]+\\)[+]?}$" in
        (** does command end in the literal {[0-9]+} ? **)
        if i < 0 then (
          printf "line not ending in literal\n%!";
          Buffer.add_string buffer buff;
          Buffer.add_string buffer "\r\n";
          return (`Ok)
        ) else (
          (** literal's size **)
          let len = int_of_string (Str.matched_group 1 buff) in
          (** buffer's content up to the literal **)
          let sub = Str.string_before buff i in
          let literal = Str.string_after buff i in
          Buffer.add_string buffer sub;
          printf "line is ending in literal %d %s --%s--\n%!" len literal sub;
          if match_regex (Buffer.contents buffer) append_regex then (
            printf "handling append\n%!";
            Buffer.add_string buffer literal;
            Buffer.add_string buffer "\r\n";
            return (`Ok)
          ) else (
            printf "handling another command with the literal\n%!";
            (** request more data from the client
             * have to do some sanity check to make sure the client
             * is not flooding with data/invalid commands, commands have
             * only so many arguments that could be the literal,
             * also check the literal's size TBD
            **)
            (if match_regex literal "[+]}$" = false then
              write_resp writer (Resp_Cont("")));

              read_literal reader len (fun res buff -> 
                Buffer.add_string buffer buff;
              ) >>= function
              | `Ok -> read_network reader writer buffer
              | `Eof _ -> return (`Eof)
          )
        )

let rec handle_client_requests contexts reader writer =
  pr_state contexts;
  let buffer = Buffer.create 0 in
  read_network reader writer buffer >>= function
    | `Eof -> printf "connection closed\n%!";return ()
    | `Ok -> let buff = Buffer.contents buffer in printf "read buff --%s--\n%!" buff;
      let context = get_request_context contexts buff in
      (match context with 
      | Ok (ctx) -> handle_commands reader writer contexts ctx
      | Error e -> (return_resp_ctx (Some (contexts.state_ctx)) (Resp_Bad(None, e)) None)) >>= 
        fun {resp_state_ctx;resp_ctx;resp_mbx_ctx} ->
        if resp_state_ctx = (Some State_Logout) then
        (
          printf "disconnecting\n%!";
          return()
        )
        else
        (
          handle_response writer context resp_ctx;
          printf "updating contexts and recursing into handle_client_requests\n%!";
          let new_contexts = 
            ({req_ctx = update_contexts contexts context resp_ctx;
             state_ctx = (match resp_state_ctx with Some s->s|None->contexts.state_ctx);
             mbx_ctx = (match resp_mbx_ctx with Some m->m|None-> contexts.mbx_ctx)}) in
          handle_client_requests new_contexts reader writer
        )
