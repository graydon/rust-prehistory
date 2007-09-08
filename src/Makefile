
LIBS=nums.cma unix.cma bigarray.cma

OCAMLCFLAGS:=-g

# list them in link order, please!
MLS:=ast.ml val.ml ll1parser.ml lexer.ml trace.ml interp.ml main.ml 
CMOS:=$(patsubst %.ml, %.cmo, $(MLS))

main: $(CMOS)
	ocamlc $(OCAMLCFLAGS) -o $@ $(LIBS) $^ 

.phony: clean

clean:
	rm -f *.cmo *.cmi lexer.ml 

%.cmo %.cmi: %.ml
	ocamlc -c $(OCAMLCFLAGS) $<

%.cmi: %.mli
	ocamlc -c $(OCAMLCFLAGS) $<

%.ml: %.mll
	ocamllex $<

%.ml %.mli: %.mly
	ocamlyacc -v $<

.depend: $(MLS) Makefile
	ocamldep $(MLS) > .depend

include .depend
