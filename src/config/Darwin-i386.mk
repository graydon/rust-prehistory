# This assumes you're on an OSX x86 host. 

RUNTIME_OBJS:=$(patsubst %.c, %.o, $(RUNTIME_CS))
RUNTIME:=librustrt.so

all: $(COMPILER) $(RUNTIME) Makefile

$(RUNTIME): $(RUNTIME_OBJS) Makefile
	gcc -shared -o $@ -fPIC $(RUNTIME_OBJS)

%.o: %.c Makefile
	gcc -c -o $@ -fPIC $<
