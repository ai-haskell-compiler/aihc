all:
	ghc -O2 -ddump-simpl-stats -no-recomp Unit.hs --make
	./Unit
	ghc -O2 pi.hs --make
	./pi 10000000

hpc:
	ghc --make -i. -i../ -optc-DMEXP=19937 -optc-DNDEBUG -optc-O3 -optc-finline-functions -optc-fomit-frame-pointer -optc-fno-strict-aliasing -optc--param -optcmax-inline-insns-single=1800 -#include "SFMT.h" -package base-3.0.1.0 -package old-time-1.0.0.0 -O -Wall -O2 -fvia-C -XCPP -XForeignFunctionInterface -XBangPatterns Unit.hs -o Unit -fhpc -no-recomp  ../dist/build/cbits/SFMT.o -I../include

clean:
	rm -f *~ Unit *.hi *.o
