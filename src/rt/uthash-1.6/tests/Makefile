HASHDIR = ../src
FUNCS = BER SAX FNV OAT JEN SFH MUR
UTILS = emit_keys
PROGS = test1 test2 test3 test4 test5 test6 test7 test8 test9   \
		    test10 test11 test12 test13 test14 test15 test16 test17 \
        test18 test19 test20 test21 test22 test23 test24 test25 \
        test26 test27 test28 test29 test30 test31 test32 test33 \
        test34 test35 test36 test37 test38 test39
CFLAGS = -I$(HASHDIR) 
CFLAGS += -O3
#CFLAGS += -g
CFLAGS += -Wall 
CFLAGS += ${EXTRA_CFLAGS}

ifeq ($(HASH_DEBUG),1)
CFLAGS += -DHASH_DEBUG=1
endif

ifeq ($(HASH_PEDANTIC),1)
CFLAGS += -pedantic 
endif

TEST_TARGET=run_tests
TESTS=./do_tests

# Prefer 64-bit compilation on Mac OS X (not necessary, just faster)
ifneq ($(strip $(shell $(CC) -v 2>&1 |egrep "i[0-9]+-apple-darwin")),)
  CFLAGS += -m64
endif

# detect Cygwin
ifneq ($(strip $(shell $(CC) -v 2>&1 |grep "cygwin")),)
  TESTS=./do_tests.cygwin
endif

# detect MinGW
ifneq ($(strip $(shell $(CC) -v 2>&1 |grep "mingw")),)
  TEST_TARGET=run_tests_mingw
  TESTS=./do_tests.mingw
endif



all: $(PROGS) $(UTILS) $(FUNCS) $(TEST_TARGET)

tests_only: $(PROGS) $(TEST_TARGET)

debug:
	$(MAKE) all HASH_DEBUG=1

pedantic:
	$(MAKE) all HASH_PEDANTIC=1

cplusplus:
	CC=$(CXX) $(MAKE) all 

example: example.c $(HASHDIR)/uthash.h
	$(CC) $(CFLAGS) -o $@ $(@).c 

$(PROGS) $(UTILS) : $(HASHDIR)/uthash.h
	$(CC) $(CFLAGS) -o $@ $(@).c 

$(FUNCS) : $(HASHDIR)/uthash.h
	$(CC) $(CFLAGS) -DHASH_FUNCTION=HASH_$@ -o keystat.$@ keystat.c 

run_tests: $(PROGS)
	perl $(TESTS)

run_tests_mingw: $(PROGS)
	/bin/sh do_tests.mingw

.PHONY: clean

clean:	
	rm -f $(UTILS) $(PROGS) test*.out keystat.??? example *.exe
	rm -rf *.dSYM
