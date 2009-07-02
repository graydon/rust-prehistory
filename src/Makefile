######################################################################
# Auto-configuration
######################################################################

CFG_OSTYPE := $(shell uname -s)
CFG_CPUTYPE := $(shell uname -m)

CFG_GCC_COMPILE_FLAGS :=
CFG_GCC_LINK_FLAGS :=

ifeq ($(CFG_OSTYPE), Linux)
  CFG_RUNTIME := librustrt.so
  CFG_GCC_COMPILE_FLAGS += -fPIC
  CFG_GCC_LINK_FLAGS += -fPIC -ldl
  ifeq ($(CFG_CPUTYPE), x86_64)
    CFG_GCC_COMPILE_FLAGS += -m32
    CFG_GCC_LINK_FLAGS += -m32
  endif
  CFG_NATIVE := 1
  CFG_UNIXY := 1
endif

ifeq ($(CFG_OSTYPE), Darwin)
  CFG_RUNTIME := librustrt.dylib
  CFG_UNIXY := 1
endif

ifneq ($(findstring, MINGW, $(CFG_OSTYPE)), )
  CFG_WINDOWSY := 1
endif

ifdef CFG_WINDOWSY
  CFG_PATH_MUNGE := | sed -e 's/\\\(.\)/\/\1/g'
  CFG_RUNTIME := rustrt.dll
  CFG_OBJ_SUFFIX := .o
  CFG_EXE_SUFFIX := .exe
  CFG_GCC_LINK_FLAGS += -fPIC
  CFG_RUN_TARG = $(1)
  # FIXME: support msvc at some point
  CFG_GCC := 1
endif

ifdef CFG_UNIXY
  CFG_COMPILER := ./rustc
  CFG_OBJ_SUFFIX := .o
  CFG_RUN_TARG = LD_LIBRARY_PATH=. $(1)
  CFG_GCC := 1
endif

ifdef CFG_GCC
  CFG_GCC_COMPILE_FLAGS += -Wall -Werror -pedantic -std=c99 -g
  CFG_GCC_LINK_FLAGS += -shared -g
  CFG_COMPILE_C = gcc $(CFG_GCC_COMPILE_FLAGS) -c -o $(1) $(2)
  CFG_LINK_C = gcc $(CFG_GCC_LINK_FLAGS) -o $(1)
  CFG_DEPEND_C = gcc -MT "$(1)" -MM $(2)
endif


######################################################################
# Basic build targets and rules
######################################################################

MKFILES := Makefile
ML_INCS := -I fe -I me -I be -I driver -I util
ML_LIBS := unix.cma nums.cma
ML_NATIVE_LIBS := unix.cmxa nums.cmxa
OCAMLC_FLAGS := -g $(ML_INCS)
OCAMLOPT_FLAGS := $(ML_INCS)

# List them in link order.
# Nobody calculates the link-order DAG automatically, sadly.
UTIL_BOT_MLS := util/common.ml util/bitv.ml
DRIVER_BOT_MLS := driver/session.ml
BE_MLS := be/asm.ml be/il.ml be/abi.ml be/x86.ml be/ra.ml be/pe.ml be/elf.ml be/macho.ml
ME_MLS := me/walk.ml me/semant.ml me/resolve.ml me/alias.ml me/auto.ml \
          me/type.ml me/mode.ml me/typestate.ml me/layout.ml me/trans.ml be/dwarf.ml
FE_MLS := fe/ast.ml fe/ll1parser.ml fe/lexer.ml
DRIVER_TOP_MLS := driver/main.ml

COMPILER_MLS := $(UTIL_BOT_MLS) $(DRIVER_BOT_MLS) $(BE_MLS) $(FE_MLS) $(ME_MLS) $(DRIVER_TOP_MLS)
COMPILER_CMOS := $(COMPILER_MLS:.ml=.cmo)
COMPILER_CMXS := $(COMPILER_MLS:.ml=.cmx)
COMPILER_CMIS := $(COMPILER_MLS:.ml=.cmi)

RUNTIME_CS := rt/rust.c rt/isaac/randport.c
RUNTIME_INCS := -Irt/uthash-1.6/src -Irt/isaac -Irt/uthash
RUNTIME_OBJS := $(RUNTIME_CS:.c=$(CFG_OBJ_SUFFIX))
RUNTIME_LIBS := $(CFG_RUNTIME_LIBS)

all: $(CFG_COMPILER) $(CFG_RUNTIME) $(MKFILES) fe/lexer.ml

loc:
	wc -l $(COMPILER_MLS) $(RUNTIME_CS)

$(CFG_RUNTIME): $(RUNTIME_OBJS) $(MKFILES)
	$(call CFG_LINK_C, $@) $(RUNTIME_OBJS)

%$(CFG_OBJ_SUFFIX): %.c $(MKFILES)
	$(call CFG_COMPILE_C, $@, $(RUNTIME_INCS)) $<

ifdef CFG_NATIVE
$(CFG_COMPILER): $(COMPILER_CMXS) $(MKFILES)
	ocamlopt.opt -o $@ $(OCAMLOPT_FLAGS) $(ML_NATIVE_LIBS) $(COMPILER_CMXS)
else
$(CFG_COMPILER): $(COMPILER_CMOS) $(MKFILES)
	ocamlc.opt -o $@ $(OCAMLC_FLAGS) $(ML_LIBS) $(COMPILER_CMOS)
endif

%.cmo: %.cmi $(MKFILES)

%.cmi: %.mli $(MKFILES)
	ocamlc.opt -c -o $@ $(OCAMLC_FLAGS) $<

%.cmo: %.ml $(MKFILES)
	ocamlc.opt -c -o $@ $(OCAMLC_FLAGS) $<

%.cmx: %.ml $(MKFILES)
	ocamlopt.opt -c -o $@ $(OCAMLOPT_FLAGS) $<

%.ml: %.mll $(MKFILES)
	ocamllex.opt -o $@ $<


######################################################################
# Auto-dependency
######################################################################

ML_DEPFILES:=$(COMPILER_MLS:.ml=.d)
C_DEPFILES:=$(RUNTIME_CS:.c=.d)

%.d: %.c $(MKFILES)
	$(call CFG_DEPEND_C, $@ $(patsubst %.c, %$(CFG_OBJ_SUFFIX), $<), $(RUNTIME_INCS)) $< $(CFG_PATH_MUNGE) >$@

%.d: %.ml $(MKFILES)
	ocamldep $(ML_INCS) $< $(CFG_PATH_MUNGE) >$@

%.d: %.mli $(MKFILES)
	ocamldep $(ML_INCS) $< $(CFG_PATH_MUNGE) >$@

-include $(ML_DEPFILES) $(C_DEPFILES)


######################################################################
# Testing
######################################################################

TEST_RUN_PASS_CRATES:=$(wildcard test/run-pass/*.rc)
TEST_RUN_PASS_SOURCES:=$(wildcard test/run-pass/*.rs)
TEST_RUN_PASS_EXES:=$(TEST_RUN_PASS_CRATES:.rc=$(CFG_EXE_SUFFIX)) \
                    $(TEST_RUN_PASS_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_RUN_PASS_OUTS:=$(TEST_RUN_PASS_EXES:$(CFG_EXE_SUFFIX)=.out)


TEST_RUN_FAIL_CRATES:=$(wildcard test/run-fail/*.rc)
TEST_RUN_FAIL_SOURCES:=$(wildcard test/run-fail/*.rs)
TEST_RUN_FAIL_EXES:=$(TEST_RUN_FAIL_CRATES:.rc=$(CFG_EXE_SUFFIX)) \
                    $(TEST_RUN_FAIL_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_RUN_FAIL_OUTS:=$(TEST_RUN_FAIL_EXES:$(CFG_EXE_SUFFIX)=.out)


TEST_COMPILE_FAIL_CRATES:=$(wildcard test/compile-fail/*.rc)
TEST_COMPILE_FAIL_SOURCES:=$(wildcard test/compile-fail/*.rs)
TEST_COMPILE_FAIL_EXES:=$(TEST_COMPILE_FAIL_CRATES:.rc=$(CFG_EXE_SUFFIX)) \
                        $(TEST_COMPILE_FAIL_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_COMPILE_FAIL_OUTS:=$(TEST_COMPILE_FAIL_EXES:$(CFG_EXE_SUFFIX)=.out)


check: $(TEST_RUN_PASS_EXES) $(TEST_RUN_FAIL_EXES) \
       $(TEST_RUN_PASS_OUTS) $(TEST_RUN_FAIL_OUTS) \
       $(TEST_COMPILE_FAIL_OUTS)

test/run-pass/%.out: test/run-pass/%$(CFG_EXE_SUFFIX) $(CFG_RUNTIME)
	$(call CFG_RUN_TARG, $<) 2>&1 | tee $@

test/run-fail/%.out: test/run-fail/%$(CFG_EXE_SUFFIX) test/run-fail/%.pat $(CFG_RUNTIME)
	rm -f $@
	$(call CFG_RUN_TARG, $<) >$@ 2>&1 || true
	grep --line-regexp --text --quiet --file=$(@:.out=.pat) $@

test/run-pass/%$(CFG_EXE_SUFFIX): test/run-pass/%.rc test/run-pass/%.rs $(CFG_COMPILER) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $@ $<
	chmod 0755 $@

test/run-fail/%$(CFG_EXE_SUFFIX): test/run-fail/%.rc test/run-pass/%.rs $(CFG_COMPILER) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $@ $<
	chmod 0755 $@

test/compile-fail/%.out: test/compile-fail/%.rc test/run-pass/%.rs test/compile-fail/%.pat $(CFG_COMPILER) $(CFG_RUNTIME)
	rm -f $@
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $(@:.out=$(CFG_EXE_SUFFIX)) $< >$@ 2>&1 || true
	grep --line-regexp --text --quiet --file=$(@:.out=.pat) $@

test/run-pass/%$(CFG_EXE_SUFFIX): test/run-pass/%.rs $(CFG_COMPILER) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $@ $<
	chmod 0755 $@

test/run-fail/%$(CFG_EXE_SUFFIX): test/run-fail/%.rs $(CFG_COMPILER) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $@ $<
	chmod 0755 $@

test/compile-fail/%.out: test/compile-fail/%.rs test/compile-fail/%.pat $(CFG_COMPILER) $(CFG_RUNTIME)
	rm -f $@
	OCAMLRUNPARAM="b1" $(CFG_COMPILER) -o $(@:.out=$(CFG_EXE_SUFFIX)) $< >$@ 2>&1 || true
	grep --line-regexp --text --quiet --file=$(@:.out=.pat) $@


######################################################################
# Cleanup
######################################################################

.phony: clean

clean:
	rm -f $(RUNTIME_OBJS) $(COMPILER_CMOS) $(COMPILER_CMIS) $(COMPILER_CMXS)
	rm -f $(ML_DEPFILES) $(C_DEPFILES)
	rm -f fe/lexer.ml
	rm -f $(CFG_COMPILER) $(CFG_RUNTIME)
	rm -f $(TEST_RUN_PASS_EXES) $(TEST_RUN_PASS_OUTS)
	rm -f $(TEST_RUN_FAIL_EXES) $(TEST_RUN_FAIL_OUTS)
	rm -f $(TEST_COMPILE_FAIL_EXES) $(TEST_COMPILE_FAIL_OUTS)
