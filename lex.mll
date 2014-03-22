{
open Core.Std
open Lexing
open Parser
open Printf

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let first = ref true

let debug frmt = printf frmt (**(fun format a -> ())**)

let print_first f = if !f = true then printf "first is true\n" else printf "first is false\n"

let match_date str =
let dq = "\"" in
let quote re = dq ^ re ^ dq in
let group re = "\\(" ^ re ^ "\\)" in
let list_of re = "(" ^ re ^ ")" in
let orx re1 re2 = re1 ^ "\\|" ^ re2 in
let astring = "[^\r\n{()%*\"]+" in
let quote_spec_char = "[\\\"]" in
let quoted_char = "[^\r\n\\\"]" in
let qstring = quote ( ( group ( orx quote_spec_char quoted_char ) ) ^ "+" ) in
let mon = group "Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec" in
let dd = group ( orx ( group " [0-9]") (group "[0-9][0-9]")) in
let yyyy = group "[0-9][0-9][0-9][0-9]" in
let time = group "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" in
let zone = group "[+-][0-9][0-9][0-9][0-9]" in
let date = group ("\"" ^ dd ^ "-" ^ mon ^ "-" ^ yyyy ^ " " ^ time ^ " " ^ zone ^ "\"") in
  try
    (Str.search_forward (Str.regexp date) str 0) >= 0
  with _ -> false


let flags_table = String.Table.create()
    let _ = List.iter 
  [
   "Deleted"            ,FLDELETED;
   "Answered"           ,FLANSWERED;
   "Draft"              ,FLDRAFT;
   "Flagged"            ,FLFLAGGED;
   "Seen"               ,FLSEEN;
  ] (fun (kwd, tok) -> Hashtbl.add_exn flags_table kwd tok)


let keyword_table = String.Table.create()
    let _ = List.iter 
 [ "ALL"		,ALL;
   "ANSWERED"		,ANSWERED;
   "APPEND"		,APPEND;
   "AUTHENTICATE"	,AUTHENTICATE;
   "EXAMINE"		,EXAMINE;
   "EXPUNGE"		,EXPUNGE;
   "EXTERNAL"		,EXTERNAL;
   "BCC"		,BCC;
   "BEFORE"		,BEFORE;
   "BODY"	        ,BODY;
   "BODYSTRUCTURE"	,BODYSTRUCTURE;
   "CAPABILITY"		,CAPABILITY;
   "CC"			,CC;
   "CHARSET"		,CHARSET;
   "CHECK"		,CHECK;
   "CLOSE"		,CLOSE;
   "COPY"		,COPY;
   "CREATE"		,CREATE;
   "DELETE"		,DELETE;
   "DELETED"		,DELETED;
   "DRAFT"		,DRAFT;
   "ENVELOPE"		,ENVELOPE;
   "FAST"		,FAST;
   "FETCH"		,FETCH;
   "FLAGGED"		,FLAGGED;
   "FLAGS"		,FLAGS;
   "-FLAGS"		,FLAGSMIN;
   "+FLAGS"		,FLAGSPL;
   "FLAGS.SILENT"	,FLAGSSILENT;
   "-FLAGS.SILENT"	,FLAGSSILENTMIN;
   "+FLAGS.SILENT"	,FLAGSSILENTPL;
   "FROM"		,FROM;
   "FULL"		,FULL;
   "GSSAPI"		,GSSAPI;
   "HEADER"		,HEADER;
   "INTERNALDATE"	,INTERNALDATE;
   "ID"			,ID;
   "IDLE"		,IDLE;
   "INBOX"		,INBOX;
   "KERBEROS_V4"	,KERBEROS_V4;
   "KEYWORD"		,KEYWORD;
   "LARGER"		,LARGER;
   "LIST"		,LIST;
   "LSUB"		,LSUB;
   "LOGIN"		,LOGIN;
   "LOGOUT"		,LOGOUT;
   "MESSAGES"		,MESSAGES;
   "NEW"		,NEW;
   "NIL"		,NIL;
   "NOT"		,NOT;
   "NOOP"		,NOOP;
   "OLD"		,OLD;
   "ON"			,ON;
   "OR"			,OR;
   "PLAIN"		,PLAIN;
   "RECENT"		,RECENT;
   "RENAME"		,RENAME;
   "RFC822"		,RFC822;
   "RFC822.HEADER"	,RFC822HEADER;
   "RFC822.SIZE"	,RFC822SIZE;
   "RFC822.TEXT"	,RFC822TEXT;
   "SEARCH"		,SEARCH;
   "SEEN"		,SEEN;
   "SELECT"		,SELECT;
   "SENTBEFORE"		,SENTBEFORE;
   "SENTON"		,SENTON;
   "SENTSINCE"		,SENTSINCE;
   "SINCE"		,SINCE;
   "SKEY"		,SKEY;
   "SMALLER"		,SMALLER;
   "STARTTLS"		,STARTTLS;
   "STATUS"		,STATUS;
   "STORE"		,STORE;
   "SUBJECT"		,SUBJECT;
   "TEXT"		,TEXT;
   "TO"			,TO;
   "SUBSCRIBE"		,SUBSCRIBE;
   "UID"		,UID;
   "UIDNEXT"		,UIDNEXT;
   "UIDVALIDITY"	,UIDVALIDITY;
   "UNANSWERED"		,UNANSWERED;
   "UNDELETED"		,UNDELETED;
   "UNDRAFT"		,UNDRAFT;
   "UNFLAGGED"		,UNFLAGGED;
   "UNKEYWORD"		,UNKEYWORD;
   "UNSUBSCRIBE"	,UNSUBSCRIBE;
   "UNSEEN"		,UNSEEN  ] (fun (kwd, tok) -> Hashtbl.add_exn keyword_table kwd tok)
}

let number = ['0'-'9']+
let nz_number = ['1'-'9'] ['0'-'9']*
let crlf = "\r\n"
let quoted_chars = ([^'\r' '\n' '\"' '\\'] | ('\\' '\\') | ('\\' '\"'))*
let atom_chars =    ([^'(' ')' '{' ' ' '%' '*' '\"' '\\' ']' '\r' '\n'])+
let astring_chars = ([^'(' ')' '{' ' ' '%' '*' '\"' '\\' ']' '\r' '\n'] | ']')+
let tag =           ([^'(' ')' '{' ' ' '%' '*' '\"' '\\' ']' '\r' '\n' '+'] | ']')+ 
let anychar =       ([^'\n' '\r' ' ' '\"' '\\' '(' ')' '{' ']'])+
let done = ['d' 'D'] ['o' 'O'] ['n' 'N'] ['e' 'E'] ['\r'] ['\n']
let body = (['b' 'B'] ['o' 'O'] ['d' 'D'] ['y' 'Y'] '[' [^']']* ']' 
  (['<'] (number ('.' nz_number)?)? ['>'])?)
let bodypeek = (['b' 'B'] ['o' 'O'] ['d' 'D'] ['y' 'Y'] '.' ['p' 'P'] ['e' 'E'] ['e' 'E'] ['k' 'K'] '[' [^']']*']'
  (['<'] (number ('.' nz_number)?)?['>'])?)
  
rule read =
  parse
  | '\r' '\n'  			{ debug "l:CRLF %d\n%!" 1; first := true; CRLF }
  | eof      			{ debug "l:EOF\n%!"; EOF }
  | body as b                   { debug "l:body %s\n%!" b; BODYFETCH(b) }
  | bodypeek as b               { debug "l:body.peek %s\n%!" b; BODYPEEK(b) }
  | '['				{ debug "l:[\n%!"; LBK}
  | '{'				{ debug "l:]\n%!"; LBC }
  | '('				{ debug "l:(\n%!"; LP}
  | ']'				{ debug "l:]\n%!"; RBK}
  | '}'				{ debug "l:}\n%!"; RBC}
  | ')'				{ debug "l:)\n%!"; RP}
  | ' '    			{ debug "l:SP\n%!"; SP }
  | '{' (['0'-'9']+ as n) '+' '}'   { debug "l:literal-plus %s\n%!" n; LITERALPL(int_of_string(n)) }
  | '{' (['0'-'9']+ as n) '}'   { debug "l:literal %s\n%!" n; LITERAL(int_of_string(n)) }
  | '\"' quoted_chars '\"' as qs
                                { debug "l:qs %s\n%!" qs; 
                                  if match_date qs then
                                    DATE(qs)
                                  else
                                    QUOTED_STRING (qs) }
  | done                        { debug "l:DONE\n%!"; DONE }
  | anychar as cmd              {debug "l:maybe cmd %s\n%!" cmd;
                                if !first = false then 
                                  (try Hashtbl.find_exn keyword_table (String.uppercase cmd) 
                                  with Not_found -> debug "l:command not found %s\n%!" cmd; ATOM_CHARS (cmd))
                                else 
                                  (debug "l:tag %s\n" cmd; first := false; TAG (cmd))
                                }
  | '\\' (atom_chars as fl)       { debug "l:flag %s\n%!" fl; try Hashtbl.find_exn flags_table fl
                                  with Not_found -> 
                                    FLEXTENSION (fl)
                                }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
