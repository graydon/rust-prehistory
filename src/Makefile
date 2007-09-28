
LIBS=nums.cma unix.cma bigarray.cma

OCAMLCFLAGS:=-g

# list them in link order, please!
MLS:=ast.ml val.ml ll1parser.ml lexer.ml trace.ml interp.ml main.ml 
CMOS:=$(patsubst %.ml, %.cmo, $(MLS))
CMXS:=$(patsubst %.ml, %.cmx, $(MLS))

mainx: $(CMXS)
	ocamlopt.opt -o $@ $(LIBS) $^ 

main: $(CMOS)
	ocamlc.opt $(OCAMLCFLAGS) -o $@ $(LIBS) $^ 

.phony: clean

clean:
	rm -f *.cmo *.cmi lexer.ml 

%.cmo: %.ml
	ocamlc.opt -c $(OCAMLCFLAGS) $<

%.cmx: %.ml
	ocamlopt.opt -c $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	ocamlyacc -v $<

.depend: $(MLS) Makefile
	ocamldep $(MLS) > .depend

include .depend
