# This assumes you're on a linux x86-64 host with enough multilibs. 
# To cross-compile for x86. We don't presently generate code for x86-64.

RUNTIME_OBJS:=$(patsubst rt/%.c, %.o, $(RUNTIME_CS))
RUNTIME:=librustrt.so

all: $(COMPILER) $(RUNTIME) Makefile

$(RUNTIME): $(RUNTIME_OBJS) Makefile
	gcc -shared -o $@ -fPIC -m32 $(RUNTIME_OBJS)

%.o: rt/%.c Makefile
	gcc -c -o $@ -fPIC -m32 $<
