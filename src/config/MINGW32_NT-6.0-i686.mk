# This assumes you're on a win32 x86 host with gcc.

CFG_COMPILER:=rustc.exe
CFG_RUNTIME:=rustrt.dll
CFG_OBJ_SUFFIX:=.o
CFG_EXE_SUFFIX:=.exe

CFG_RUN_TARG=$(1)
CFG_COMPILE_C=gcc -Wall -Werror -pedantic -std=c99 -g -c -o $(1)
CFG_LINK_C=gcc -shared -g -o $(1) -fPIC
CFG_DEPEND_C=gcc -MT "$(1)" -MM
