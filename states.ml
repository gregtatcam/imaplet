open Core.Std

type literal = Literal of int | LiteralPlus of int

type seq_number = Wild | Number of int

type seq_set =
  | SeqNumber of seq_number
  | SeqRange of seq_number * seq_number 

type sequence = seq_set list

type responseCode = 
  | RespCode_Alert
  | RespCode_Badcharset
  | RespCode_Capability
  | RespCode_Parse
  | RespCode_Permanentflags
  | RespCode_Read_only
  | RespCode_Read_write
  | RespCode_Trycreate
  | RespCode_Uidnext
  | RespCode_Uidvalidity
  | RespCode_Unseen

type state = 
 | State_Notauthenticated
 | State_Authenticated
 | State_Selected
 | State_Logout

type statusOpt = 
  | Stat_Messages
  | Stat_Recent
  | Stat_Uidnext
  | Stat_Uidvalidity
  | Stat_Unseen
  
type authtype = 
  | Auth_Kerberos_v4
  | Auth_Gssapi
  | Auth_Skey
  | Auth_External
  | Auth_Plain
  
type searchKey = 
  | Search_All (** all messages in the mailbox; the default initial key for ANDing **)
  | Search_Answered (** messages with the \Answered flag set **)
  | Search_Bcc of string (** messages with string in the envelope sructure's BCC field **) 
  | Search_Before of Date.t (** messages with internal date **) 
  | Search_Body of string (** messages containing string in teh body **) 
  | Search_Cc of string (** messages with string int the envelope structure's CC field **)
  | Search_Deleted (** with \Deleted flag set **) 
  | Search_Draft (** with \Draft flag set **)
  | Search_Flagged (** with \Flagged flag set **)
  | Search_From of string (** messages with string in the envelope structure's FROM field **)
  | Search_Header of string * string (** messages with the header with the specified filed name * specified string **)
  | Search_Keyword of string (** messages with the keyword flag set **)
  | Search_Larger of int (** messages with the size larger than specified **) 
  | Search_New (** messages with \Recent set but not \Seen **)
  | Search_Old (** message with no \Recent flag set **) 
  | Search_On of Date.t (** messages with internal date within the specified date **)
  | Search_Recent (** messages with \Recent flag set **)
  | Search_Seen (** message with \Seen flag set **)
  | Search_Sentbefore of Date.t (** messages with Date: header is earlier **)
  | Search_Senton of Date.t (** messages with Date: header is within **)
  | Search_Sentsince of Date.t (** messages with Date: header is within or later **)
  | Search_SeqSet of sequence (** messages with the sequence numbers **)
  | Search_Since of Date.t (** messages with internal date within or later **)
  | Search_Smaller of int (** messages with size smaller **)
  | Search_Subject of string (** messages with envelope structure's SUBJECT field **)
  | Search_Text of string (** messages with the string in the header or body, could be literal **) 
  | Search_To of string (** messages with the envelope structure's TO field **)
  | Search_UID of sequence (** messages with unique identifier; is it a number? **)
  | Search_Unanswered (** messages with \Answered flag not set **)
  | Search_Undeleted (** messages with \Deleted flag not set **)
  | Search_Undraft (** messages with \Draft flag not set **)
  | Search_Unflagged (** messages with \Flagged flag not set **)
  | Search_Unkeyword of string (** message that do not have the specified keyword flag set **)
  | Search_Unseen (** messages with \Seen flag not set **)

type 'a searchKeys =
  | Key of 'a
  | KeyList of 'a searchKeys list
  | OrKey of 'a searchKeys * 'a searchKeys
  | NotKey of 'a searchKeys 
  
type fetchMacro = 
  | Fetch_All
  | Fetch_Fast
  | Fetch_Full
  
type sectionMsgtext =
  | Header
  | HeaderFields of string list
  | HeaderFieldsNot of string list
  | Text

type sectionPart = int list

type sectionText =
  | SectionMsgtext of sectionMsgtext 
  | Mime

type sectionSpec = 
  | SectionMsgtext of sectionMsgtext option
  | SectionPart of sectionPart * (sectionText option)

type bodyPart = int list (** 0,1,2 **)

type fetchAtt =
  | Fetch_Body
  | Fetch_BodySection of sectionSpec * bodyPart
  | Fetch_BodyPeekSection of sectionSpec  * bodyPart
  | Fetch_Bodystructure 
  | Fetch_Envelope 
  | Fetch_Flags
  | Fetch_Internaldate
  | Fetch_Rfc822
  | Fetch_Rfc822Header 
  | Fetch_Rfc822Size
  | Fetch_Rfc822Text
  | Fetch_Uid

type fetch =
  | FetchMacro of fetchMacro
  | FetchAtt of fetchAtt list
  
type flags =  
  | Flags_Answered
  | Flags_Deleted
  | Flags_Draft
  | Flags_Flagged
  | Flags_Recent
  | Flags_Seen
  | Flags_Keyword of string
  | Flags_Extention of string

type searchFlags =
  | Common of flags
  | NotCommon of flags
  | Old
  | New
  
type storeFlags = 
  | Store_Flags
  | Store_FlagsSilent
  | Store_PlusFlags
  | Store_PlusFlagsSilent
  | Store_MinusFlags
  | Store_MinusFlagsSilent
  
type anyCmd = 
  | Cmd_Id of string list
  | Cmd_Capability
  | Cmd_Noop
  | Cmd_Logout (** close connection **)
  
type notAuthenticatedCmd =  
  | Cmd_Starttls (** start tls negotiation **)
  | Cmd_Authenticate of authtype * string (** authentication mechanism **)
  | Cmd_Login of string * string (** user * password **)
  
type authenticatedCmd =  
  | Cmd_Select of string (** mailbox name **) 
  | Cmd_Examine of string (** mailbox name **) 
  | Cmd_Create of string (** mailbox name **)
  | Cmd_Delete of string (** mailbox name **)
  | Cmd_Rename of string * string (** existing mailbox name * new mailbox name **)
  | Cmd_Subscribe of string (** mailbox name **)
  | Cmd_Unsubscribe of string (** mailbox name **)
  | Cmd_List of string * string (** reference name * mailbox name with possible wildcards **)
  | Cmd_Lsub of string * string (** reference name * mailbox name with possible wildcards **)
  | Cmd_Status of string * (statusOpt list) (** mailbox name * status data itme names **)
  | Cmd_Append of string * flags list option * Time.t option * literal (** mailbox name * optional flag parenthesized list * optional date/time string; message literal **)
  | Cmd_Idle
  | Cmd_Done

type selectedCmd =  
  | Cmd_Check (** request a checkpoint - housekeeping, implementation dependant **)
  | Cmd_Close (** transition to authenticated state **)
  | Cmd_Expunge (** permanently remove all messages with \Deleted flag **)
  | Cmd_Search of string option * (searchKey) searchKeys * bool (** optional charset * searching criteria; charset and criteria need more grammar definition TBD **)
  | Cmd_Fetch of sequence *  fetch * bool (** more work is needed TBD **)
  | Cmd_Store of sequence * storeFlags * flags list * bool 
  | Cmd_Copy of sequence * string * bool (** sequence * mailbox name **)

type fromClient = 
  | Any of anyCmd
  | Notauthenticated of notAuthenticatedCmd
  | Authenticated of authenticatedCmd
  | Selected of selectedCmd
  | Done

type response = 
 | Resp_Ok of responseCode option * string
 | Resp_Bad of responseCode option * string
 | Resp_No of responseCode option * string
 | Resp_Bye of responseCode option * string
 | Resp_Preauth of responseCode option * string
 | Resp_Cont of string 
 | Resp_Untagged of string
 | Resp_Any of string

(** get the mailbox flags TBD **)
let answered = 0x01
let flagged = 0x02
let deleted = 0x04
let seen = 0x08
let draft = 0x10
let notJunk = 0x20
let extention = 0x30
let keyword = 0x40
let recent = 0x50

let fl_to_i fl =
  match fl with
  | Flags_Answered -> answered
  | Flags_Flagged -> flagged
  | Flags_Deleted -> deleted
  | Flags_Seen -> seen
  | Flags_Recent -> recent
  | Flags_Draft -> draft
  | Flags_Extention e -> extention (** TBD **)
  | Flags_Keyword k -> keyword (** TBD **)

let fl_to_str fl =
  match fl with
  | Flags_Answered -> "\\Answered"
  | Flags_Flagged -> "\\Flagged"
  | Flags_Deleted -> "\\Deleted"
  | Flags_Seen -> "\\Seen"
  | Flags_Recent -> "\\Recent"
  | Flags_Draft -> "\\Draft"
  | Flags_Extention e -> "\\" ^ e
  | Flags_Keyword k -> k

let str_to_fl fl =
  if Regex.match_regex fl "^\\Answered$" then
    Flags_Answered
  else if Regex.match_regex fl "^\\Flagged$" then
    Flags_Flagged
  else if Regex.match_regex fl "^\\Deleted$" then
    Flags_Deleted
  else if Regex.match_regex fl "^\\Flags$" then
    Flags_Seen
  else if Regex.match_regex fl "^\\Flags$" then
    Flags_Recent
  else if Regex.match_regex fl "^\\Draft$" then
    Flags_Draft
  else if Regex.match_regex fl "^\\Extention (.+)$" then
    Flags_Extention (Str.matched_group 1 fl)
  else 
    Flags_Keyword (fl)

let lstr_to_fl fl =
  List.fold fl ~init:[] ~f:(fun acc fl -> (str_to_fl fl) :: acc)

let lfl_to_str fl =
  List.fold fl ~init:[] ~f:(fun acc fl -> (fl_to_str fl) :: acc)

let pr_flag fl =
  match fl with
  | Flags_Answered -> printf "Answered %!"
  | Flags_Flagged -> printf "Flagged %!"
  | Flags_Deleted -> printf "Deleted %!"
  | Flags_Seen -> printf "Seen %!"
  | Flags_Recent -> printf "Recent %!"
  | Flags_Draft -> printf "Draft %!"
  | Flags_Extention e -> printf "Extention %s%!" e
  | Flags_Keyword k -> printf "Keywords %s%!" k

let pr_flags = function
  | None -> ()
  | Some l -> List.iter l ~f:(fun i -> pr_flag i); printf "\n%!"

let get_flags (fl:flags list) =
  List.fold fl ~init:0 ~f:(fun acc i -> acc land fl_to_i i)

let all_flags = answered land flagged land deleted land seen land notJunk land extention land keyword
