imaplet
=======
This is a very rough hack of IMAP server, mostly exercise in learning OCaml and IMAP related RFCs.
Grammar for all base RFC3501rev4 commands is supported, but not all functionality. There is no consideration
for error handling or efficiency. The index file of the mailbox is stored in .imaplet folder. There is
a block of 1K of data for each e-mail message (see index.ml). SSL and TLS are not supported. The plain
authentication is supported via "users" file which is included with the source. The INBOX location is
/var/mail/"user" and other mailboxes are in /Users/"user"/mail (see configure.ml). MAC email client does
pick up folders and message content. I'll be working in the next few weeks on refactoring of most of
the modules starting with index and amailbox first, to make the server configurable for different storage models.
