imaplet
=======
This is a prototype of IMAP server.
Grammar for all base RFC3501rev4 commands is supported, but not all functionality. There is no consideration
for error handling or efficiency. The index file of the mailboxes is stored in .imaplet folder. 
SSL and STARTTLS are supported. The plain authentication is supported via "users" file which is included with the source. 
The INBOX location is /var/mail/"user" and other mailboxes are in /Users/"user"/mail. Some configuraiton options are provided via imaplet.cf. Irminsule is the only supported storage type. There is conversion for Irminsule store from mbox - imaplet_irmin_build. Run it as:
imaplet_irmin_build.native -u user, where the user is the email account. imaplet_irmin_read is a simple interractive UI to access 
Irminsule store. 
Dependencies are:
async,
async_kernel,
async_unix,
bin_prot.syntax,
comparelib.syntax,
extlib,
email_message.109.42.alpha1,
irmin.backend,
irmin.unix,
lwt,
lwt.unix,
lwt.syntax,
sexplib.syntax,
str,
tls.
Use "opam install" to install dependencies.
To build all components and utilities run "make build". Depending on your computer security settings you may have to run 
executables as sudo: sudo imaplet.
