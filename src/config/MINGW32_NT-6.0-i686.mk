# This assumes you're on a win32 x86 host. 

RUNTIME_OBJS:=$(patsubst %.c, %.o, $(RUNTIME_CS))
RUNTIME:=rustrt.dll

all: $(COMPILER) $(RUNTIME) $(MKFILES)

$(RUNTIME): $(RUNTIME_OBJS) $(MKFILES)
	gcc -shared -o $@ -fPIC $(RUNTIME_OBJS)

%.o: %.c $(MKFILES)
	gcc -Wall -Werror -pedantic -std=c99 -c -o $@ $<
