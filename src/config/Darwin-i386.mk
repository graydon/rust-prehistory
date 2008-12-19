# This assumes you're on an OSX x86 host. 

CFG_COMPILER:=rustc
CFG_RUNTIME:=librustrt.dylib
CFG_OBJ_SUFFIX:=.o
CFG_EXE_SUFFIX:=

CFG_COMPILE_C=gcc -Wall -Werror -pedantic -std=c99 -fPIC -g -c -o $(1)
CFG_LINK_C=gcc -shared -g -o $(1) -fPIC
CFG_DEPEND_C=gcc -MT "$(1)" -MM

