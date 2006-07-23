

all: parser.cmo parser.cmi lexer.cmo ast.cmo 

.phony: clean

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli

parser.cmo: parser.ml parser.cmi ast.cmo
	ocamlc -c $<

lexer.cmo: lexer.ml parser.cmi 
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc $<
