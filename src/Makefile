
LIBS=nums.cma unix.cma bigarray.cma

OCAMLCFLAGS:=-g

main: ll1parser.cmo lexer.cmo val.cmo ast.cmo interp.cmo asm.cmo main.cmo 
	ocamlc $(OCAMLCFLAGS) -o $@ $(LIBS) $^ 

main.cmo: lexer.cmi interp.cmi # parser.cmi 

val.cmi val.cmo: ast.cmi

interp.cmi interp.cmo: val.cmi ast.cmi interp.ml ll1parser.cmi

.phony: clean

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli

ll1parser.cmo ll1parser.cmi: ll1parser.ml ast.cmi ast.cmo
	ocamlc -c $(OCAMLCFLAGS) $<

# parser.cmo: parser.ml parser.cmi ast.cmi ast.cmo
# 	ocamlc -c $<

# parser.cmi: parser.mli ast.cmi ast.cmo
# 	ocamlc -c $<

lexer.cmo lexer.cmi: lexer.ml ll1parser.cmi ast.cmi
	ocamlc -c $(OCAMLCFLAGS) $<

%.cmo: %.ml
	ocamlc -c $(OCAMLCFLAGS) $<

%.cmi: %.ml
	ocamlc -c $(OCAMLCFLAGS) $<

%.cmi: %.mli
	ocamlc -c $(OCAMLCFLAGS) $<

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc -v $<
