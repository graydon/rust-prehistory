
LIBS=nums.cma unix.cma bigarray.cma

main: ll1parser.cmo lexer.cmo val.cmo ast.cmo interp.cmo asm.cmo # main.cmo 
	ocamlc -o $@ $(LIBS) $^ 

main.cmo: lexer.cmi interp.cmi # parser.cmi 

interp.cmo: ast.cmi interp.ml 

.phony: clean

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli

ll1parser.cmo ll1parser.cmi: ll1parser.ml ast.cmi ast.cmo
	ocamlc -c $<

# parser.cmo: parser.ml parser.cmi ast.cmi ast.cmo
# 	ocamlc -c $<

# parser.cmi: parser.mli ast.cmi ast.cmo
# 	ocamlc -c $<

lexer.cmo lexer.cmi: lexer.ml ll1parser.cmi ast.cmi
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
