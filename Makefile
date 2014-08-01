EXE=[imaplet.native,imaplet_irmin_store.native,imaplet_irmin_build.native,imaplet_irmin_read.native,imaplet_lmtp.native,imaplet_proxy.native]

SRV_SRC = imaplet_irmin_store.ml* serverConfig.ml* storage/irminStorage.ml* storage/storageMeta.ml

STORAGE_SRC = storage/block.ml* storage/irminSrvIpc.ml* storage/irminStorage.ml* storage/irminStorageClnt.ml* storage/irminStorageCmd.ml storage/irminStorageConfig.ml* storage/mflags.ml* storage/primitives.ml* storage/storage.ml* storage/storageMeta.ml*

IMAPLET_SRC = imaplet.ml account.ml* amailbox.ml* configuration.ml* connection.ml* contextlist.ml* contexts.ml* fetchregex.ml interpreter.ml* lex.mll parser.mly regex.ml* response.ml* server.ml* serverConfig.ml* state.ml* states.ml utils.ml* 

build: imaplet imaplet_irmin_store imaplet_irmin_build imaplet_irmin_read imaplet_lmtp imaplet_proxy

clean:
	ocamlbuild -clean
	rm -rf _build setup.ml setup.data _tags

imaplet:$(STORAGE_SRC) $(IMAPLET_SRC) 
	ocamlbuild -use-ocamlfind -Is storage -tag "syntax(camlp4o)" -use-menhir -tag thread -package extlib,str,async,email_message,async_unix,async_kernel,sexplib,sexplib.syntax imaplet.native

imaplet_irmin_store: $(SRV_SRC)
	ocamlbuild -use-ocamlfind -Is storage -tag thread -tag "syntax(camlp4o)" -package core,lwt,lwt.unix,lwt.syntax,irmin.backend,irmin.unix,sexplib.syntax,comparelib.syntax,bin_prot.syntax,email_message,extlib,str imaplet_irmin_store.native

imaplet_irmin_build: $(STORAGE_SRC) serverConfig.ml*
	ocamlbuild -use-ocamlfind -Is storage -tag thread -tag "syntax(camlp4o)" -package async,email_message,async_unix,str,extlib,sexplib.syntax imaplet_irmin_build.native

imaplet_irmin_read: imaplet_irmin_read.ml storage/irminStorage.ml* storage/storageMeta.ml*
	ocamlbuild -use-ocamlfind -Is storage -tag thread -tag "syntax(camlp4o)" -package lwt,irmin.backend,irmin.unix,sexplib.syntax,comparelib.syntax,bin_prot.syntax,core,str,email_message,extlib imaplet_irmin_read.native

imaplet_lmtp: imaplet_lmtp.ml lmtpConfig.ml* serverConfig.ml* regex.ml*
	ocamlbuild -use-ocamlfind -tag thread -tag "syntax(camlp4o)" -package core,lwt,lwt.unix,lwt.syntax,extlib,str imaplet_lmtp.native

imaplet_proxy: imaplet_proxy.ml serverConfig.ml* 
	ocamlbuild -use-ocamlfind -tag thread -tag "syntax(camlp4o)" -package core,lwt,lwt.unix,lwt.syntax,tls.lwt,str,cstruct.unix imaplet_proxy.native
