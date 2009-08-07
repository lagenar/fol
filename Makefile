all: fol.cmo lexer.cmo parser.cmo
	ocamlc -c main.ml
	ocamlc -o main fol.cmo lexer.cmo parser.cmo main.cmo
fol.cmo: fol.ml
	ocamlc -c fol.ml
lexer.ml: lexer.mll
	ocamllex lexer.mll
parser.ml: parser.mly
	ocamlyacc parser.mly
	ocamlc -c parser.mli
parser.cmo: parser.ml
	ocamlc -c parser.ml
lexer.cmo: lexer.ml parser.cmo
	ocamlc -c lexer.ml
clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml *~ \#*