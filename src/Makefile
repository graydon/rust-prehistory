######################################################################
# Auto-configuration
######################################################################

CFG_OSTYPE := $(shell uname -s)
CFG_CPUTYPE := $(shell uname -m)

CFG_INFO := $(info building on $(CFG_OSTYPE) $(CFG_CPUTYPE))

CFG_GCC_COMPILE_FLAGS :=
CFG_GCC_LINK_FLAGS :=
CFG_VALGRIND :=

ifeq ($(CFG_OSTYPE), Linux)
  CFG_RUNTIME := librustrt.so
  CFG_GCC_COMPILE_FLAGS += -fPIC
  CFG_GCC_LINK_FLAGS += -shared -fPIC -ldl
  ifeq ($(CFG_CPUTYPE), x86_64)
    CFG_GCC_COMPILE_FLAGS += -m32
    CFG_GCC_LINK_FLAGS += -m32
  endif
  CFG_NATIVE := 1
  CFG_UNIXY := 1
  #CFG_VALGRIND := $(shell which valgrind)
  ifdef CFG_VALGRIND
    CFG_VALGRIND += --run-libc-freeres=no --leak-check=full --quiet --vex-iropt-level=0
  endif
endif

ifeq ($(CFG_OSTYPE), Darwin)
  CFG_RUNTIME := librustrt.dylib
  CFG_UNIXY := 1
  CFG_GCC_LINK_FLAGS += -dynamiclib
  # Darwin has a very blurry notion of "64 bit", and claims it's running
  # "on an i386" when the whole userspace is 64-bit and the compiler
  # emits 64-bit binaries by default. So we just force -m32 here. Smarter
  # approaches welcome!
  CFG_GCC_COMPILE_FLAGS += -m32
  CFG_GCC_LINK_FLAGS += -m32
endif

ifneq ($(findstring MINGW,$(CFG_OSTYPE)),)
  CFG_WINDOWSY := 1
endif

ifdef CFG_WINDOWSY
  CFG_INFO := $(info windows-y environment)
  CFG_PATH_MUNGE := | sed -e 's/\\\(.\)/\/\1/g'
  CFG_RUNTIME := rustrt.dll
  CFG_OBJ_SUFFIX := .o
  CFG_EXE_SUFFIX := .exe
  CFG_BOOT := ./rustboot.exe
  CFG_COMPILER := ./rustc.exe
  CFG_GCC_LINK_FLAGS += -shared -fPIC
  CFG_RUN_TARG = $(1)
  # FIXME: support msvc at some point
  CFG_GCC := 1
endif

ifdef CFG_UNIXY
  CFG_INFO := $(info unix-y environment)
  CFG_BOOT := ./rustboot
  CFG_COMPILER := ./rustc
  CFG_OBJ_SUFFIX := .o
  CFG_RUN_TARG = LD_LIBRARY_PATH=. $(CFG_VALGRIND) $(1)
  CFG_GCC := 1
endif

ifdef CFG_GCC
  CFG_INFO := $(info using gcc)
  CFG_GCC_COMPILE_FLAGS += -Wall -Werror -fno-rtti -fno-exceptions -g
  CFG_GCC_LINK_FLAGS += -g
  CFG_COMPILE_C = g++ $(CFG_GCC_COMPILE_FLAGS) -c -o $(1) $(2)
  CFG_LINK_C = g++ $(CFG_GCC_LINK_FLAGS) -o $(1)
  CFG_DEPEND_C = g++ -MT "$(1)" -MM $(2)
else
  CFG_ERR := $(error please try on a system with gcc)
endif


MKFILES := Makefile


######################################################################
# Boot targets and rules
######################################################################

ML_INCS := -I boot/fe -I boot/me -I boot/be -I boot/driver -I boot/util
ML_LIBS := unix.cma nums.cma
ML_NATIVE_LIBS := unix.cmxa nums.cmxa
OCAMLC_FLAGS := -g $(ML_INCS) -w Ael -warn-error Ael
OCAMLOPT_FLAGS := $(ML_INCS) -w Ael -warn-error Ael

# List them in link order.
# Nobody calculates the link-order DAG automatically, sadly.

UTIL_BOT_MLS := $(addprefix boot/util/, common.ml bitv.ml)
DRIVER_BOT_MLS := $(addprefix boot/driver/, session.ml)
BE_MLS := $(addprefix boot/be/, asm.ml x86.ml ra.ml pe.ml elf.ml \
          macho.ml)
IL_MLS := $(addprefix boot/be/, il.ml abi.ml)
ME_MLS := $(addprefix boot/me/, walk.ml semant.ml resolve.ml alias.ml auto.ml \
          type.ml typestate.ml mode.ml mutable.ml gc.ml layout.ml trans.ml dwarf.ml)
FE_MLS := $(addprefix boot/fe/, ast.ml parser.ml lexer.ml)
DRIVER_TOP_MLS := $(addprefix boot/driver/, main.ml)

BOOT_MLS := $(UTIL_BOT_MLS) $(DRIVER_BOT_MLS) $(FE_MLS) $(IL_MLS) $(ME_MLS) $(BE_MLS) \
            $(DRIVER_TOP_MLS)
BOOT_CMOS := $(BOOT_MLS:.ml=.cmo)
BOOT_CMXS := $(BOOT_MLS:.ml=.cmx)
BOOT_OBJS := $(BOOT_MLS:.ml=.o)
BOOT_CMIS := $(BOOT_MLS:.ml=.cmi)

RUNTIME_CS := rt/rust.cpp rt/isaac/randport.cpp
RUNTIME_INCS := -Irt/isaac -Irt/uthash
RUNTIME_OBJS := $(RUNTIME_CS:.cpp=$(CFG_OBJ_SUFFIX))
RUNTIME_LIBS := $(CFG_RUNTIME_LIBS)

all: $(CFG_COMPILER) $(MKFILES) boot/fe/lexer.ml

loc:
	wc -l $(BOOT_MLS) $(RUNTIME_CS) rt/rust.h

$(CFG_RUNTIME): $(RUNTIME_OBJS) $(MKFILES) rt/rust.h
	$(call CFG_LINK_C, $@) $(RUNTIME_OBJS)

%$(CFG_OBJ_SUFFIX): %.cpp $(MKFILES)
	$(call CFG_COMPILE_C, $@, $(RUNTIME_INCS)) $<

ifdef CFG_NATIVE
$(CFG_BOOT): $(BOOT_CMXS) $(MKFILES)
	ocamlopt.opt -o $@ $(OCAMLOPT_FLAGS) $(ML_NATIVE_LIBS) $(BOOT_CMXS)
else
$(CFG_BOOT): $(BOOT_CMOS) $(MKFILES)
	ocamlc.opt -o $@ $(OCAMLC_FLAGS) $(ML_LIBS) $(BOOT_CMOS)
endif

%.cmi: %.mli $(MKFILES)
	ocamlc.opt -c -o $@ $(OCAMLC_FLAGS) $<

%.cmo: %.ml $(MKFILES)
	ocamlc.opt -c -o $@ $(OCAMLC_FLAGS) $<

%.cmo: %.cmi $(MKFILES)

%.cmx %.o: %.ml $(MKFILES)
	ocamlopt.opt -c -o $@ $(OCAMLOPT_FLAGS) $<

%.ml: %.mll $(MKFILES)
	ocamllex.opt -o $@ $<


######################################################################
# Main compiler targets and rules
######################################################################

UTIL_BOT_RSS := $(addprefix comp/util/, common.rs bitv.rs)
DRIVER_BOT_RSS := $(addprefix comp/driver/, session.rs)
BE_RSS := $(addprefix comp/be/, asm.rs il.rs abi.rs x86.rs ra.rs pe.rs elf.rs \
          macho.rs)
ME_RSS := $(addprefix comp/me/, walk.rs semant.rs resolve.rs alias.rs auto.rs \
          type.rs mode.rs typestate.rs layout.rs trans.rs dwarf.rs)
FE_RSS := $(addprefix comp/fe/, ast.rs parser.rs lexer.rs)
DRIVER_TOP_RSS := $(addprefix comp/driver/, rustc.rs)

COMPILER_RSS := $(wildcard $(UTIL_BOT_RSS) $(DRIVER_BOT_RSS) $(BE_RSS) $(FE_RSS) $(ME_RSS) $(DRIVER_TOP_RSS))

$(CFG_COMPILER): comp/rustc.rc $(COMPILER_RSS) $(CFG_BOOT) $(CFG_RUNTIME)
	echo $(COMPILER_RSS)
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $@ $<
	chmod 0755 $@

self: $(CFG_COMPILER)
	$(call CFG_RUN_TARG, $(CFG_COMPILER)) comp/rustc.rc


######################################################################
# Auto-dependency
######################################################################

ML_DEPFILES := $(BOOT_MLS:%.ml=%.d)
C_DEPFILES := $(RUNTIME_CS:%.cpp=%.d)

%.d: %.cpp $(MKFILES)
	$(call CFG_DEPEND_C, $@ $(patsubst %.cpp, %$(CFG_OBJ_SUFFIX), $<), $(RUNTIME_INCS)) $< $(CFG_PATH_MUNGE) >$@

%.d: %.ml $(MKFILES)
	ocamldep $(ML_INCS) $< $(CFG_PATH_MUNGE) >$@

%.d: %.mli $(MKFILES)
	ocamldep $(ML_INCS) $< $(CFG_PATH_MUNGE) >$@

-include $(ML_DEPFILES) $(C_DEPFILES)


######################################################################
# Testing
######################################################################

TEST_XFAILS := test/run-pass/many.rs \
               test/run-pass/spawn-fn.rs \
               test/run-pass/mlist_cycle.rs \
               test/run-pass/acyclic-unwind.rs \
               test/run-pass/linear-for-loop.rs \
               $(wildcard test/run-fail/*.rs) \
               test/compile-fail/infinite-tag-type-recursion.rs \
               test/compile-fail/infinite-vec-type-recursion.rs


TEST_RUN_PASS_CRATES := $(filter-out $(TEST_XFAILS), $(wildcard test/run-pass/*.rc))
TEST_RUN_PASS_SOURCES := $(filter-out $(TEST_XFAILS), $(wildcard test/run-pass/*.rs))
TEST_RUN_PASS_EXES := $(TEST_RUN_PASS_CRATES:.rc=$(CFG_EXE_SUFFIX)) \
                      $(TEST_RUN_PASS_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_RUN_PASS_OUTS := $(TEST_RUN_PASS_EXES:$(CFG_EXE_SUFFIX)=.out)


TEST_RUN_FAIL_CRATES := $(filter-out $(TEST_XFAILS), $(wildcard test/run-fail/*.rc))
TEST_RUN_FAIL_SOURCES := $(filter-out $(TEST_XFAILS), $(wildcard test/run-fail/*.rs))
TEST_RUN_FAIL_PATS := $(wildcard test/run-fail/*.pat)
TEST_RUN_FAIL_EXES := $(TEST_RUN_FAIL_CRATES:.rc=$(CFG_EXE_SUFFIX)) \
                      $(TEST_RUN_FAIL_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_RUN_FAIL_OUTS := $(TEST_RUN_FAIL_EXES:$(CFG_EXE_SUFFIX)=.out)


TEST_COMPILE_FAIL_CRATES := $(filter-out $(TEST_XFAILS), $(wildcard test/compile-fail/*.rc))
TEST_COMPILE_FAIL_SOURCES := $(filter-out $(TEST_XFAILS), $(wildcard test/compile-fail/*.rs))
TEST_COMPILE_FAIL_PATS := $(wildcard test/compile-fail/*.pat)
TEST_COMPILE_FAIL_EXES := $(TEST_COMPILE_FAIL_CRATES:.rc=$(CFG_EXE_SUFFIX)) \
                          $(TEST_COMPILE_FAIL_SOURCES:.rs=$(CFG_EXE_SUFFIX))
TEST_COMPILE_FAIL_OUTS := $(TEST_COMPILE_FAIL_EXES:$(CFG_EXE_SUFFIX)=.out)


check_nocompile: $(TEST_COMPILE_FAIL_OUTS)

check: $(TEST_RUN_PASS_EXES) $(TEST_RUN_FAIL_EXES) \
       $(TEST_RUN_PASS_OUTS) $(TEST_RUN_FAIL_OUTS) \
       $(TEST_COMPILE_FAIL_OUTS)

test/run-pass/%.out: test/run-pass/%$(CFG_EXE_SUFFIX) $(CFG_RUNTIME)
	$(call CFG_RUN_TARG, $<) > $@ ; X=$$? ; cat $@ ; exit $$X

test/run-fail/%.out: test/run-fail/%$(CFG_EXE_SUFFIX) test/run-fail/%.pat $(CFG_RUNTIME)
	rm -f $@
	$(call CFG_RUN_TARG, $<) >$@ 2>&1 || true
	grep --line-regexp --text --quiet --file=$(@:.out=.pat) $@

test/run-pass/%$(CFG_EXE_SUFFIX): test/run-pass/%.rc test/run-pass/%.rs $(CFG_BOOT) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $@ $<
	chmod 0755 $@

test/run-fail/%$(CFG_EXE_SUFFIX): test/run-fail/%.rc test/run-pass/%.rs $(CFG_BOOT) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $@ $<
	chmod 0755 $@

test/compile-fail/%.out: test/compile-fail/%.rc test/run-pass/%.rs test/compile-fail/%.pat $(CFG_BOOT) $(CFG_RUNTIME)
	rm -f $@
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $(@:.out=$(CFG_EXE_SUFFIX)) $< >$@ 2>&1 || true
	grep --line-regexp --text --quiet --file=$(@:.out=.pat) $@

test/run-pass/%$(CFG_EXE_SUFFIX): test/run-pass/%.rs $(CFG_BOOT) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $@ $<
	chmod 0755 $@

test/run-fail/%$(CFG_EXE_SUFFIX): test/run-fail/%.rs $(CFG_BOOT) $(CFG_RUNTIME)
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $@ $<
	chmod 0755 $@

test/compile-fail/%.out: test/compile-fail/%.rs test/compile-fail/%.pat $(CFG_BOOT) $(CFG_RUNTIME)
	rm -f $@
	OCAMLRUNPARAM="b1" $(CFG_BOOT) -o $(@:.out=$(CFG_EXE_SUFFIX)) $< >$@ 2>&1 || true
	grep --line-regexp --text --quiet --file=$(@:.out=.pat) $@


######################################################################
# Distribution
######################################################################

PKG_NAME := rust
PKG_VER  := $(shell date +"%Y-%m-%d")-snap
PKG_DIR := $(PKG_NAME)-$(PKG_VER)
PKG_TAR := $(PKG_DIR).tar.gz
PKG_3RDPARTY := rt/valgrind.h \
                rt/isaac/rand.h rt/isaac/standard.h \
                rt/uthash/uthash.h rt/uthash/utlist.h \
                rt/bigint/bigint.h rt/bigint/bigint_int.cpp \
                rt/bigint/bigint_ext.cpp rt/bigint/low_primes.h
PKG_FILES := README COPYING\
             $(MKFILES) $(BOOT_MLS) boot/fe/lexer.mll \
             comp/rustc.rc $(COMPILER_RSS) \
             $(RUNTIME_CS) rt/rust.h $(PKG_3RDPARTY) \
             $(TEST_RUN_PASS_CRATES) $(TEST_RUN_PASS_SOURCES) \
             $(TEST_RUN_FAIL_CRATES) $(TEST_RUN_FAIL_SOURCES) $(TEST_RUN_FAIL_PATS) \
             $(TEST_COMPILE_FAIL_CRATES) $(TEST_COMPILE_FAIL_SOURCES) $(TEST_COMPILE_FAIL_PATS)

dist:
	mkdir -p dist/$(PKG_DIR)
	tar -c $(PKG_FILES) | tar -x -C dist/$(PKG_DIR)
	tar -czf $(PKG_TAR) -C dist $(PKG_DIR)
	rm -Rf dist

distcheck:
	rm -Rf $(PKG_NAME)-*.tar.gz dist
	mkdir -p dist/$(PKG_DIR)
	tar -c $(PKG_FILES) | tar -x -C dist/$(PKG_DIR)
	make -C dist/$(PKG_DIR) check
	make -C dist/$(PKG_DIR) clean
	tar -czf $(PKG_TAR) -C dist $(PKG_DIR)
	rm -Rf dist
	@echo
	@echo -----------------------------------------------
	@echo $(PKG_TAR) ready for distribution
	@echo -----------------------------------------------


######################################################################
# Cleanup
######################################################################

.phony: clean

clean:
	rm -f $(RUNTIME_OBJS) $(BOOT_CMOS) $(BOOT_CMIS) $(BOOT_CMXS) $(BOOT_OBJS)
	rm -f $(CFG_COMPILER)
	rm -f $(ML_DEPFILES) $(C_DEPFILES)
	rm -f boot/fe/lexer.ml
	rm -f $(CFG_BOOT) $(CFG_RUNTIME)
	rm -f $(TEST_RUN_PASS_EXES) $(TEST_RUN_PASS_OUTS)
	rm -f $(TEST_RUN_FAIL_EXES) $(TEST_RUN_FAIL_OUTS)
	rm -f $(TEST_COMPILE_FAIL_EXES) $(TEST_COMPILE_FAIL_OUTS)
	rm -Rf $(PKG_NAME)-*.tar.gz dist
