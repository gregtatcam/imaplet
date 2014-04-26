type mailboxFlags =  
  | Flags_Answered
  | Flags_Deleted
  | Flags_Draft
  | Flags_Flagged
  | Flags_Recent
  | Flags_Seen
  | Flags_Keyword of string
  | Flags_Extention of string
  | Flags_Template
