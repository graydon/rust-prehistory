OSTYPE:=$(shell uname -s)
CPUTYPE:=$(shell uname -m)
CONFIGMK:=config/$(OSTYPE)-$(CPUTYPE).mk
include $(CONFIGMK)

MKFILES:=Makefile $(CONFIGMK)

ML_INCS:=-I fe -I me -I be -I driver -I util
ML_LIBS=unix.cma nums.cma
OCAMLC_FLAGS:=-g $(ML_INCS)

# List them in link order.
# Nobody calculates the link-order DAG automatically, sadly.
UTIL_BOT_MLS:=util/common.ml util/bitv.ml
DRIVER_BOT_MLS:=driver/session.ml
BE_MLS:=be/asm.ml be/il.ml be/abi.ml be/x86.ml be/ra.ml be/pe.ml be/elf.ml be/macho.ml
ME_MLS:=me/walk.ml me/semant.ml me/resolve.ml me/alias.ml me/auto.ml me/type.ml me/typestate.ml me/layout.ml me/trans.ml be/dwarf.ml
FE_MLS:=fe/ast.ml fe/ll1parser.ml fe/lexer.ml
DRIVER_TOP_MLS:=driver/main.ml

COMPILER_MLS:=$(UTIL_BOT_MLS) $(DRIVER_BOT_MLS) $(BE_MLS) $(FE_MLS) $(ME_MLS) $(DRIVER_TOP_MLS)
COMPILER_CMOS:=$(COMPILER_MLS:.ml=.cmo)
COMPILER_CMIS:=$(COMPILER_MLS:.ml=.cmi)

RUNTIME_CS:=rt/rust.c
RUNTIME_OBJS:=$(RUNTIME_CS:.c=$(CFG_OBJ_SUFFIX))

all: $(CFG_COMPILER) $(CFG_RUNTIME) $(MKFILES) fe/lexer.ml

$(CFG_RUNTIME): $(RUNTIME_OBJS) $(MKFILES)
	$(call CFG_LINK_C,$@) $(RUNTIME_OBJS)

%$(CFG_OBJ_SUFFIX): %.c $(MKFILES)
	$(call CFG_COMPILE_C,$@) $<

$(CFG_COMPILER): $(COMPILER_CMOS) $(MKFILES)
	ocamlc -o $@ $(OCAMLC_FLAGS) $(ML_LIBS) $(COMPILER_CMOS)

%.cmo: %.cmi $(MKFILES)

%.cmi: %.mli $(MKFILES)
	ocamlc -c -o $@ $(OCAMLC_FLAGS) $<

%.cmo: %.ml $(MKFILES)
	ocamlc -c -o $@ $(OCAMLC_FLAGS) $<

%.ml: %.mll $(MKFILES)
	ocamllex -o $@ $<

# Auto-dependency logic

ML_DEPFILES:=$(COMPILER_MLS:.ml=.d)
C_DEPFILES:=$(RUNTIME_CS:.c=.d)

%.d: %.c $(MKFILES)
	$(call CFG_DEPEND_C,$@ $(patsubst %.c,%$(CFG_OBJ_SUFFIX),$<)) $< | sed -e 's/\\\(.\)/\/\1/g' >$@

%.d: %.ml $(MKFILES)
	ocamldep $(ML_INCS) $< | sed -e 's/\\\(.\)/\/\1/g' >$@

%.d: %.mli $(MKFILES)
	ocamldep $(ML_INCS) $< | sed -e 's/\\\(.\)/\/\1/g' >$@

-include $(ML_DEPFILES) $(C_DEPFILES)

# Testing logic

TEST_CRATES:=$(wildcard test/*.rc)
TEST_SOURCES:=$(wildcard test/*.rs)
TEST_EXES:=$(TEST_CRATES:.rc=$(CFG_EXE_SUFFIX)) $(TEST_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_OUTS:=$(TEST_EXES:$(CFG_EXE_SUFFIX)=.out)

check: $(TEST_EXES) $(TEST_OUTS)

test/%.out: test/%$(CFG_EXE_SUFFIX) $(CFG_RUNTIME)
	$< 2>&1 | tee $@

test/%$(CFG_EXE_SUFFIX): test/%.rc $(CFG_COMPILER) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $@ $<

test/%$(CFG_EXE_SUFFIX): test/%.rs $(CFG_COMPILER) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $@ $<

# Cleanup logic

.phony: clean

clean:
	rm -f $(RUNTIME_OBJS) $(COMPILER_CMOS) $(COMPILER_CMIS)
	rm -f $(ML_DEPFILES) $(C_DEPFILES)
	rm -f fe/lexer.ml
	rm -f $(CFG_COMPILER) $(CFG_RUNTIME)
	rm -f $(TEST_EXES) $(TEST_OUTS)
