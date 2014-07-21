SRV_SRC = irminStorageSrv.ml* storage/irminStorage.ml* storage/storageMeta.ml

STORAGE_SRC = storage/block.ml* storage/irminSrvIpc.ml* storage/irminStorage.ml* storage/irminStorageClnt.ml* storage/irminStorageCmd.ml storage/irminStorageConfig.ml* storage/mflags.ml* storage/primitives.ml* storage/storage.ml* storage/storageMeta.ml*

IMAPLET_SRC = imaplet.ml account.ml* amailbox.ml* configuration.ml* connection.ml* contextlist.ml* contexts.ml* fetchregex.ml interpreter.ml* lex.mll parser.mly regex.ml* response.ml* server.ml* state.ml* states.ml utils.ml* 

all: imaplet srv store read_store lmtp

clean:
	ocamlbuild -clean
	rm -rf _build 

imaplet:$(STORAGE_SRC) $(IMAPLET_SRC) 
	corebuild -Is storage -cflag -annot -tag debug -verbose 6 -use-menhir -tag thread -use-ocamlfind -quiet -package extlib,str,async,email_message,async_unix,async_kernel,sexplib imaplet.native

srv: $(SRV_SRC)
	corebuild -Is storage -use-ocamlfind -no-hygiene -tag thread -tag "syntax(camlp4o)" -package core,lwt,lwt.unix,lwt.syntax,irmin.backend,irmin.unix,sexplib.syntax,comparelib.syntax,bin_prot.syntax,email_message,extlib,str irminStorageSrv.native

store: $(STORAGE_SRC)
	corebuild -Is storage -pkgs async,email_message,async_unix,str,extlib build_irmin_store.native

read_store: read_store.ml storage/irminStorage.ml* storage/storageMeta.ml*
	ocamlbuild -Is storage -use-ocamlfind -no-hygiene -tag thread -tag "syntax(camlp4o)" -package lwt,irmin.backend,irmin.unix,sexplib.syntax,comparelib.syntax,bin_prot.syntax,core,str,email_message,extlib read_store.native

lmtp: lmtp.ml lmtpConfig.ml*
	corebuild -use-ocamlfind -no-hygiene -tag thread -tag "syntax(camlp4o)" -package core,lwt,lwt.unix,lwt.syntax,extlib,str lmtp.native
