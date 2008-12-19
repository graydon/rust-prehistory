# This assumes you're on a linux x86-64 host with enough multilibs. 
# To cross-compile for x86. We don't presently generate code for x86-64.

CFG_COMPILER:=rustc
CFG_RUNTIME:=librustrt.so
CFG_OBJ_SUFFIX:=.o
CFG_EXE_SUFFIX:=

CFG_COMPILE_C=gcc -Wall -Werror -pedantic -std=c99 -fPIC -m32 -g -c -o $(1)
CFG_LINK_C=gcc -shared -g -o $(1) -fPIC -m32
CFG_DEPEND_C=gcc -MT "$(1)" -MM
