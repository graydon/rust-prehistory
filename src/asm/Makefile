

LIBS=nums.cma unix.cma bigarray.cma

OCAMLCFLAGS:=-g

# list them in link order, please!
MLS:=asm.ml pe.ml
CMOS:=$(patsubst %.ml, %.cmo, $(MLS))
CMXS:=$(patsubst %.ml, %.cmx, $(MLS))


main: $(CMOS)
	ocamlc $(OCAMLCFLAGS) -o $@ $(LIBS) $^ 

mainx: $(CMXS)
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
