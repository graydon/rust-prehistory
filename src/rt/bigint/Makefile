# Makefile for bigint.

CC =		gcc -Wall
CFLAGS =	-O
#CFLAGS =	-O -g
LDFLAGS =	-s
#LDFLAGS =	

all:		bi_factor bi_bc


bigint.a:		bigint_ext.o bigint_int.o
	ar rc bigint.a bigint_ext.o bigint_int.o

bigint_ext.o:		bigint_ext.c bigint.h
	$(CC) $(CFLAGS) -c bigint_ext.c

bigint_int.o:		bigint_int.c bigint.h
	$(CC) $(CFLAGS) -c bigint_int.c


bi_test:		bi_test.o bigint.a
	$(CC) $(LDFLAGS) bi_test.o bigint.a -o bi_test

bi_test.o:		bi_test.c bigint.h
	$(CC) $(CFLAGS) -c bi_test.c


bi_factor:		bi_factor.o bigint.a
	$(CC) $(LDFLAGS) bi_factor.o bigint.a -o bi_factor

bi_factor.o:		bi_factor.c low_primes.h bigint.h
	$(CC) $(CFLAGS) -c bi_factor.c


bi_bc_parse.c bi_bc_parse.h:		bi_bc_parse.y
	bison -d -o bi_bc_parse.c bi_bc_parse.y
bi_bc_parse.o:		bi_bc_parse.c bigint.h
	$(CC) $(CFLAGS) -c bi_bc_parse.c

bi_bc_scan.o:		bi_bc_scan.c bigint.h bi_bc_parse.h
	$(CC) $(CFLAGS) -c bi_bc_scan.c

bi_bc:			bi_bc_parse.o bi_bc_scan.o bigint.a
	$(CC) $(LDFLAGS) bi_bc_parse.o bi_bc_scan.o bigint.a -ll -o bi_bc


clean:
	-rm -f bi_test bi_factor bi_bc *.a *.o a.out core *.core core.* bi_bc_parse.c bi_bc_parse.h
