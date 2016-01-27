f:	syntax.ml lexer.mll parser.mly main.ml
	ocamlc -c syntax.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o f lexer.cmo parser.cmo syntax.cmo main.cmo 

clean:
	rm -rf f *.mli *.cmi lexer.ml parser.ml *.cmo
