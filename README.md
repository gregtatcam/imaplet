imaplet
=======
This is a prototype of IMAP server.
Grammar for all base RFC3501rev4 commands is supported, but not all functionality. There is no consideration
for error handling or efficiency. The index file of the mailboxes is stored in .imaplet folder. 
SSL and TLS are not supported. The plain authentication is supported via "users" file which is included with the source. 
The INBOX location is /var/mail/"user" and other mailboxes are in /Users/"user"/mail. You need to update configuration.ml 
if you want to change the location. Two formats of mailboxes are supported: mbox and Irminsule. Change get_store() in configuraton.ml to choose the store type. There is conversion for Irminsule store from mbox - build_irmin_store. Run it as:
build_irmin_srv.native -u user, where the user is the email account. read_store is a simple interractive UI to access 
Irminsule store. 
email_message has to be build manualy. The archive is located here: http://opam.ocaml.org/packages/email_message/email_message.109.42.alpha1/
Other dependencies are:
async,
async_kernel,
async_unix,
bin_prot.syntax,
comparelib.syntax,
extlib,
irminsule.backend.git,
lwt,
lwt.unix,
lwt.syntax,
sexplib.syntax,
str.
Use "opam install" to install dependencies.
To build all components and utilities run "make all". Depending on your computer security settings you may have to run 
executables as sudo.
