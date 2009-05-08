# This assumes you're on a linux x86 host.

CFG_COMPILER:=./rustc
CFG_RUNTIME:=librustrt.so
CFG_OBJ_SUFFIX:=.o
CFG_EXE_SUFFIX:=

CFG_RUN_TARG=LD_LIBRARY_PATH=. $(1)
CFG_COMPILE_C=gcc -Wall -Werror -pedantic -std=c99 -g -c -o $(1)
CFG_LINK_C=gcc -shared -g -o $(1) -fPIC
CFG_DEPEND_C=gcc -MT "$(1)" -MM
