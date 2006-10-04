
LIBS=nums.cma

main: parser.cmo lexer.cmo ast.cmo interp.cmo main.cmo
	ocamlc -o $@ $(LIBS) $^ 

main.cmo: parser.cmi lexer.cmi interp.cmi

interp.cmo: ast.cmi parser.cmi interp.ml 

.phony: clean

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli

parser.cmo: parser.ml parser.cmi ast.cmi ast.cmo
	ocamlc -c $<

parser.cmi: parser.mli ast.cmi ast.cmo
	ocamlc -c $<

lexer.cmo lexer.cmi: lexer.ml parser.cmi ast.cmi
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc -v $<
