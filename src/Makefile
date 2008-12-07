ML_INCS:=-I fe -I me -I be -I driver -I util
ML_LIBS=unix.cma nums.cma
OCAMLC_FLAGS:=-g $(ML_INCS)

# list them in link order, please!
UTIL_BOT_MLS:=util/common.ml 
DRIVER_BOT_MLS:=driver/session.ml 
BE_MLS:=be/asm.ml be/il.ml be/abi.ml be/bitv.ml be/x86.ml be/ra.ml be/pe.ml be/elf.ml
ME_MLS:=me/semant.ml me/resolve.ml me/trans.ml
FE_MLS:=fe/ast.ml fe/ll1parser.ml fe/lexer.ml
DRIVER_TOP_MLS:=driver/main.ml 

COMPILER:=rustc
COMPILER_MLS:=$(UTIL_BOT_MLS) $(DRIVER_BOT_MLS) $(BE_MLS) $(FE_MLS) $(ME_MLS) $(DRIVER_TOP_MLS)
COMPILER_CMOS:=$(patsubst %.ml, %.cmo, $(COMPILER_MLS))

RUNTIME_CS:=rt/rust.c

OSTYPE:=$(shell uname -s)
CPUTYPE:=$(shell uname -m)

CONFIGMK:=config/$(OSTYPE)-$(CPUTYPE).mk
MKFILES:=Makefile $(CONFIGMK)

include $(CONFIGMK)

$(COMPILER): $(COMPILER_CMOS) $(MKFILES)
	ocamlc -o $@ $(OCAMLC_FLAGS) $(ML_LIBS) $(COMPILER_CMOS)

.phony: clean

clean:
	rm -f */*.cmo */*.cmi */*.o *.o
	rm -f fe/lexer.ml 
	rm -f $(COMPILER) $(RUNTIME)
	rm -f .depend
	rm -f rust_out.exe rust_out

%.cmo: %.cmi $(MKFILES)

%.cmi: %.mli $(MKFILES)
	ocamlc -c -o $@ $(OCAMLC_FLAGS) $<

%.cmo: %.ml $(MKFILES)
	ocamlc -c -o $@ $(OCAMLC_FLAGS) $<

%.ml: %.mll $(MKFILES)
	ocamllex -o $@ $<

.depend: $(COMPILER_MLS) $(RUNTIME_CS) $(MKFILES)
	ocamldep $(ML_INCS) $(COMPILER_MLS) > .depend
	gcc -MM $(RUNTIME_CS) >>.depend

include .depend
