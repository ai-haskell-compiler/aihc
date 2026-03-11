GHC=ghc
GHCFLAGS=-O -i../..

CC=gcc
CFLAGS=-O2

all: rdtsc_haskell rdtsc_c

rdtsc_haskell: TestRdtsc.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $<

rdtsc_c: test_rdtsc.c rdtsc.o
	$(CC) $(CFLAGS) -o $@ -I../cbits test_rdtsc.c rdtsc.o

rdtsc.o: ../cbits/rdtsc.c ../cbits/rdtsc.h
	$(CC) $(CFLAGS) -c -I../cbits $<

clean:
	rm -f *.o *.hi

really-clean: clean
	rm -f rdtsc_haskell rdtsc_c
