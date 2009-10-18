RESULT = fol
SOURCES = \
	util.ml fol.mli fol.ml cnl.mli cnl.ml parser.mly lexer.mll main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)