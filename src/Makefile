ML_INCS:=-I fe -I me -I be 
ML_LIBS=unix.cma nums.cma
OCAMLC_FLAGS:=-g $(ML_INCS)

# list them in link order, please!
BE_MLS:=be/asm.ml be/il.ml be/x86.ml be/pe.ml
ME_MLS:=me/semant.ml
FE_MLS:=fe/ast.ml fe/ll1parser.ml fe/lexer.ml 
DRIVER_MLS:=driver/main.ml 

COMPILER:=rustc
COMPILER_MLS:=$(BE_MLS) $(ME_MLS) $(FE_MLS) $(DRIVER_MLS)
COMPILER_CMOS:=$(patsubst %.ml, %.cmo, $(COMPILER_MLS))

RUNTIME_CS:=rt/rust.c
RUNTIME_OBJS:=$(patsubst %.c, %.o, $(RUNTIME_CS))
RUNTIME:=rustrt.dll

all: $(COMPILER) $(RUNTIME) 

$(COMPILER): $(COMPILER_CMOS)
	ocamlc -o $@ $(OCAMLC_FLAGS) $(ML_LIBS) $^ 

$(RUNTIME): $(RUNTIME_OBJS)
	gcc -shared -o $@ $^

.phony: clean

clean:
	rm -f */*.cmo */*.cmi */*.o 
	rm -f fe/lexer.ml 
	rm -f $(COMPILER) $(RUNTIME)
	rm -f .depend
	rm -f rust_out.exe

%.o: %.c
	gcc -c -o $@ $<

%.cmo: %.ml
	ocamlc -c -o $@ $(OCAMLC_FLAGS) $<

%.ml: %.mll
	ocamllex -o $@ $<

.depend: $(COMPILER_MLS) $(RUNTIME_CS) Makefile
	ocamldep $(ML_INCS) $(COMPILER_MLS) > .depend
	gcc -MM $(RUNTIME_CS) >>.depend

-include .depend
