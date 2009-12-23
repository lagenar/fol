RESULT = fol
SOURCES = \
	error.ml util.ml fol.mli fol.ml clause.ml cnl.mli cnl.ml parser.mly lexer.mll main.ml

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)