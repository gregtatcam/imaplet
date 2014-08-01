imaplet
=======
This is a prototype of IMAP server.
Grammar for all base RFC3501rev4 commands is supported, but not all functionality. There is no consideration
for error handling or efficiency. The index file of the mailboxes is stored in .imaplet folder. 
SSL and STARTTLS are supported. The plain authentication is supported via "users" file which is included with the source. 
The INBOX location is /var/mail/"user" and other mailboxes are in /Users/"user"/mail. Some configuraiton options are provided via imaplet.cf. Irminsule is the only supported storage type. There is conversion for Irminsule store from mbox - imaplet_irmin_build. Run it as:
imaplet_irmin_build.native -u user, where the user is the email account. imaplet_irmin_read is a simple interractive UI to access 
Irminsule store. 
email_message has to be build manualy. The archive is located here: http://opam.ocaml.org/packages/email_message/email_message.109.42.alpha1/
Other dependencies are:
async,
async_kernel,
async_unix,
bin_prot.syntax,
comparelib.syntax,
extlib,
irmin.backend,
irmin.unix,
lwt,
lwt.unix,
lwt.syntax,
sexplib.syntax,
str,
tls.
There is an open issue #176 for tls package: https://github.com/mirleft/ocaml-tls/issues. If it is not resolved you have to install asn1-combinators, x509, and tls manually. asn1-combinators requires some changes before building: rename src/core.ml to src/core1.ml then replace all references to Core with Core1 (asn.ml,asn1-combinators.[mldylib,mllib], asn_random.ml, ber_der.ml, cache.ml combinators.ml.
Use "opam install" to install dependencies.
To build all components and utilities run "make build". Depending on your computer security settings you may have to run 
executables as sudo.
