
LIBS=unix.cmxa nums.cmxa
OCAMLCFLAGS:=-g

# list them in link order, please!
MLS:=ast.ml ll1parser.ml lexer.ml main.ml 
CMOS:=$(patsubst %.ml, %.cmo, $(MLS))
CMXS:=$(patsubst %.ml, %.cmx, $(MLS))


main: $(CMXS)
	ocamlopt -o $@ $(LIBS) $^ 

.phony: clean

clean:
	rm -f *.cmo *.cmi *.o *.cmx lexer.ml 

%.cmo: %.ml
	ocamlc -c $(OCAMLCFLAGS) $<

%.cmx: %.ml
	ocamlopt -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	ocamlyacc -v $<

.depend: $(MLS) Makefile
	ocamldep $(MLS) > .depend

include .depend
