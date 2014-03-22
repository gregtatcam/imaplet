open Core.Std

exception InvalidDate

let match_regex_i ?(case=true) str regx = 
  try
    let regexp =
    (
      if case = false then
        Str.regexp_case_fold regx
      else
        Str.regexp regx
    ) in
    (Str.search_forward regexp str 0)
  with _ ->
    (-1)

let match_regex ?(case=true) str regx = 
  let i = match_regex_i ~case str regx in
  (i >= 0)

let replace regx tmpl str =
  Str.global_replace (Str.regexp regx) tmpl str

let crlf = "\r\n"

let sol = "^"

let eol = "$"

let dot = "\\."

let space = " "

let nz_number = "[1-9][0-9]*"

let number = "[0-9]+"
  
let dq = "\""

let tag = "[^\r\n{()%*\"\\ ]+"

let astring = "[^\r\n{()%*\"]+"

let quote_spec_char = "[\\\"]"

let quoted_char = "[^\r\n\\\"]"

let all_of_it re = "^" ^ re ^ "$"

let quote re = 
  if match_regex re "^\"[^\"]*\"$" then
    re
  else
    dq ^ re ^ dq

let squote re = 
  let re = replace "\"" "" re in
  let re =  dq ^ re ^ dq in
  printf "squote %s\n%!" re;
  re

let dequote re =
  replace "\"" "" re

let group re = "\\(" ^ re ^ "\\)"

let optional re = (group re) ^ "?"

let list_of re = "(" ^ re ^ ")"

let dlist_of re = "((" ^ re ^ "))"

let bkt_list_of re = "\\[" ^ re ^ "\\]"

let ang_list_of re = "<" ^ re ^ ">"

let orx re1 re2 = re1 ^ "\\|" ^ re2

let orxl l = String.concat l ~sep:"\\|"

let qstring = quote ( ( group ( orx quote_spec_char quoted_char ) ) ^ "+" )

let mon = group "Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec"

let dd = group ( orx ( group "[0-9]") (group "[0-9][0-9]"))

let dd_fixed = group ( orx ( group " [0-9]") (group "[0-9][0-9]"))

let yyyy = group "[0-9][0-9][0-9][0-9]"

let time = group "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"

let zone = group "[+-][0-9][0-9][0-9][0-9]"

let literal = "{[0-9]+[+]?}"

let day_of_week = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wen"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | 6 -> "Sat"
  | _ -> "Sun"

let of_day_of_week = function
  | "Sun" -> 0
  | "Mon" -> 1
  | "Tue" -> 2
  | "Wen" -> 3
  | "Thu" -> 4
  | "Fri" -> 5
  | "Sat" -> 6
  | _ -> 0

let month_to_int = function 
  | "Jan" -> 0 
  | "Feb" -> 1
  | "Mar" -> 2
  | "Apr" -> 3
  | "May" -> 4
  | "Jun" -> 5
  | "Jul" -> 6
  | "Aug" -> 7
  | "Sep" -> 8
  | "Oct" -> 9
  | "Nov" -> 10
  | "Dec" -> 11
  | _ -> 0

let month_to_int12 m = (month_to_int m) + 1


let int_to_month = function 
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> "Jan"

let int12_to_month i = int_to_month (i-1)

let date_regex =
  dd ^ "-" ^ mon ^ "-" ^ yyyy

let date_dqregex = quote( date_regex )

let date_time_regex = 
  dd_fixed ^ "-" ^ mon ^ "-" ^ yyyy ^ " " ^ time ^ " " ^ zone

(**Date: Mon, 7 Feb 1994 21:52:25 -0800 (PST)**) 
let email_regex =
  dd ^ " " ^ mon ^ " " ^ yyyy ^ " " ^ time ^ " " ^ zone


let date_time_dqregex =
  quote( date_time_regex)

(** convert date from IMAP string to OCaml Date.t 
 * dd-mmm-yyyy
 **)
let imapd_to_date_exn (date:string) : (Date.t) =
  let d =
  (if match_regex date date_dqregex then
    replace "\"" "" date
  else
    date
  ) in
  if match_regex d date_regex = false then (
    raise InvalidDate
  ) else (
    let parts = Str.split (Str.regexp "-") date in
    let cvtd = sprintf "%s-%02d-%02d" (List.nth_exn parts 2) (month_to_int12 (List.nth_exn parts 1)) (int_of_string (List.nth_exn parts 0)) in
    let date = Date.of_string cvtd in
    date
  )

(** figure out seconds to convert to utc **)
let utc_sec zone =
  if match_regex zone "^\\([+-]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)$" then (
    let sign = Str.matched_group 1 zone in
    let hours = int_of_string (Str.matched_group 2 zone) in
    let mins = int_of_string (Str.matched_group 3 zone) in
    if hours > 11 || mins > 40 then
      0
    else (
      let secs = (hours * 3600 + mins * 60) in
      if sign = "+" then
        secs * (-1)
      else
        secs
    )
  ) else
    0

(** convert date from append to Date: format **)
let imapd_to_date_time_exn date =
  if match_regex date date_time_dqregex = false then
    raise InvalidDate
  else (
    try
    let date = replace "\"" "" date in
    let parts = Str.split (Str.regexp " ") date in
    let date = List.nth_exn parts 0 in
    let time = List.nth_exn parts 1 in
    let zone = List.nth_exn parts 2 in
    let dparts = Str.split (Str.regexp "-") date in
    let day = List.nth_exn dparts 0 in
    let month = List.nth_exn dparts 1 in
    let year = List.nth_exn dparts 2 in
    let str = sprintf "%s-%02d-%s %s" year (month_to_int12 month) day time in
    let tm = Time.of_string str in
    let f = (Time.to_float tm) +. (Float.of_int (utc_sec zone)) in
    Time.of_float f
    with e -> raise e
  )

let date_time_to_email (dt:Time.t) : (string) =
  let tm = Unix.gmtime (Time.to_float dt) in
    (sprintf "%s, %d %s %d %02d:%02d:%02d +0000" 
    (day_of_week tm.tm_wday) tm.tm_mday (int_to_month tm.tm_mon) (tm.tm_year + 100) 
    tm.tm_hour tm.tm_min tm.tm_sec)

(** convert date/time in email format to Time.t
email:
[ day-of-week "," ] date FWS time [CFWS],
day month year
time-of-day FWS zone
hour ":" minute [ ":" second ]
Date: Mon, 7 Feb 1994 21:52:25 -0800 (PST) 
date/time: 2014-03-05 21:36:02.233417Z
**)
let email_to_date_time_exn date =
  let date = replace "\"" "" date in
  let date = replace "^[ ]*\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\), " "" date in
  if match_regex date email_regex = false then
    raise InvalidDate;
  
  let parts = Str.split (Str.regexp " ") date in
  let day = int_of_string (List.nth_exn parts 0) in
  let month = month_to_int12 (List.nth_exn parts 1) in
  let year = List.nth_exn parts 2 in
  let time = 
  if match_regex time "^[0-9][0-9]:[0-9][0-9]$" then
    List.nth_exn parts 3 ^ ":00"
  else
    List.nth_exn parts 3
  in
  let zone = List.nth_exn parts 4 in
  let str = sprintf "%s-%02d-%02d %s" year month day time in
  let tm = Time.of_string str in
  let f = (Time.to_float tm) +. (Float.of_int (utc_sec zone)) in
  Time.of_float f


let append_regex =
  let cmd = "append" in
  let mbox = group astring in
  let qmbox = group qstring in
  let mailbox = group (orx mbox qmbox) in
  let flags = group (" " ^ list_of astring) in
  let date = group (" " ^ date_time_dqregex) in
  "^" ^ tag ^ " " ^ cmd ^ " " ^ mailbox ^ flags ^ "?" ^ date ^ "? $"

(** match reserved files  **)
let match_dot file = 
  file = ".imap" || file = ".subscriptions" || file = ".subscriptions.lock" || file = ".imaplet"

(** convert imap mailbox regex to ocaml regex **)
let fixregx_mbox mailbox =
  let str = replace "\\*" ".+" mailbox in
  let str = replace "^%$" "^[^/]+$" str in
  let str = replace "^%" "^[^/]+" str in
  let str = replace "%$" "[^/]+$" str in
  replace "%" "[^/]+" str

(** regular expression with debug **)
let match_regex_d ?(case=true) str regex =
  let b = match_regex ~case str regex in
  printf "matching %s %s %b\n%!" str regex b;
  b

let trim_space (str:string) : string =
  let str = replace "^[ ]+" "" str in
  replace "[ ]+$" "" str
