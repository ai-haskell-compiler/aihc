# Makefile for percentformat
#
# Copyright:   (c) 2015-2018 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  test/spec \
  test/unit \
  test/number \
  test/scientific \
  test/quotient \
  test/prop
EGS =
BENCHS = bench/speculate
GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HUGSIMPORTDIRS = .:./src:./test:/usr/lib/hugs/packages/*
INSTALL_DEPS = leancheck

all: mk/toplibs $(TESTS)

test: $(patsubst %,%.test,$(TESTS)) diff-test

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test unit

test-via-stack:
	stack test percent-format:test:unit --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

test-sdist:
	./test/sdist

legacy-test: # needs ghc-8.0 .. ghc-7.8 installed as such
	make clean  &&  make test GHC=ghc-8.2  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-8.0  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

prepare:
	cabal update  &&  cabal install leancheck

prepare-legacy-test:
	cabal-ghc-8.2  update  &&  cabal-ghc-8.2  install leancheck
	cabal-ghc-8.0  update  &&  cabal-ghc-8.0  install leancheck
	cabal-ghc-7.10 update  &&  cabal-ghc-7.10 install leancheck
	cabal-ghc-7.8  update  &&  cabal-ghc-7.8  install leancheck

hlint:
	hlint \
	  .

doctest:
	doctest -isrc src/Text/PercentFormat.hs

bench: $(BENCHS)
	./bench/speculate

%.test: %
	./$<

clean: clean-hi-o
	rm -f $(TESTS) $(EG) mk/toplibs
	rm -rf doc/

ghci: mk/All.ghci

hugs: mk/All.hugs

hugs-test: \
  test/spec.runhugs \
  test/unit.runhugs \
  test/number.runhugs \
  test/scientific.runhugs

diff-test: examples-diff-test

update-diff-test: update-examples-diff-test

examples-diff-test: bench/examples
	./bench/examples             | diff -rud test/diff/examples.out         -
	./bench/examples "%r"        | diff -rud test/diff/examples-r.out       -
	./bench/examples "%6r"       | diff -rud test/diff/examples-6r.out      -
	./bench/examples "%-6r"      | diff -rud test/diff/examples--6r.out     -
	./bench/examples "%s"        | diff -rud test/diff/examples-s.out       -
	./bench/examples "%6s"       | diff -rud test/diff/examples-6s.out      -
	./bench/examples "%-6s"      | diff -rud test/diff/examples--6s.out     -
	./bench/examples "%i"        | diff -rud test/diff/examples-i.out       -
	./bench/examples "%3i"       | diff -rud test/diff/examples-3i.out      -
	./bench/examples "%03i"      | diff -rud test/diff/examples-03i.out     -
	./bench/examples "%-3i"      | diff -rud test/diff/examples--3i.out     -
	./bench/examples "%+3i"      | diff -rud test/diff/examples-+3i.out     -
	./bench/examples "% 3i"      | diff -rud test/diff/examples-_3i.out     -
	./bench/examples "%f"        | diff -rud test/diff/examples-f.out       -
	./bench/examples "%5f"       | diff -rud test/diff/examples-5f.out      -
	./bench/examples "%+5f"      | diff -rud test/diff/examples-+5f.out     -
	./bench/examples "%-5f"      | diff -rud test/diff/examples--5f.out     -
	./bench/examples "%11.2f"    | diff -rud test/diff/examples-11p2f.out   -
	./bench/examples "%-11.2f"   | diff -rud test/diff/examples--11p2f.out  -
	./bench/examples "%c"        | diff -rud test/diff/examples-c.out       -
	./bench/examples "%2c"       | diff -rud test/diff/examples-2c.out      -
	#./bench/examples "%b"        | diff -rud test/diff/examples-b.out       - # TODO: FIXME: infinite loop
	#./bench/examples "%o"        | diff -rud test/diff/examples-o.out       - # TODO: FIXME: infinite loop
	#./bench/examples "%x"        | diff -rud test/diff/examples-x.out       - # TODO: FIXME: infinite loop
	#./bench/examples "%X"        | diff -rud test/diff/examples-X_.out      - # TODO: FIXME: infinite loop
	./bench/examples "abc"       | diff -rud test/diff/examples-abc.out     -

update-examples-diff-test: bench/examples
	./bench/examples             > test/diff/examples.out
	./bench/examples "%r"        > test/diff/examples-r.out
	./bench/examples "%6r"       > test/diff/examples-6r.out
	./bench/examples "%-6r"      > test/diff/examples--6r.out
	./bench/examples "%s"        > test/diff/examples-s.out
	./bench/examples "%6s"       > test/diff/examples-6s.out
	./bench/examples "%-6s"      > test/diff/examples--6s.out
	./bench/examples "%i"        > test/diff/examples-i.out
	./bench/examples "%3i"       > test/diff/examples-3i.out
	./bench/examples "%03i"      > test/diff/examples-03i.out
	./bench/examples "%-3i"      > test/diff/examples--3i.out
	./bench/examples "%+3i"      > test/diff/examples-+3i.out
	./bench/examples "% 3i"      > test/diff/examples-_3i.out
	./bench/examples "%f"        > test/diff/examples-f.out
	./bench/examples "%5f"       > test/diff/examples-5f.out
	./bench/examples "%+5f"      > test/diff/examples-+5f.out
	./bench/examples "%-5f"      > test/diff/examples--5f.out
	./bench/examples "%11.2f"    > test/diff/examples-11p2f.out
	./bench/examples "%-11.2f"   > test/diff/examples--11p2f.out
	./bench/examples "%c"        > test/diff/examples-c.out
	./bench/examples "%2c"       > test/diff/examples-2c.out
	#./bench/examples "%b"        > test/diff/examples-b.out  # TODO: FIXME: infinite loop
	#./bench/examples "%o"        > test/diff/examples-o.out  # TODO: FIXME: infinite loop
	#./bench/examples "%x"        > test/diff/examples-x.out  # TODO: FIXME: infinite loop
	#./bench/examples "%X"        > test/diff/examples-X_.out # TODO: FIXME: infinite loop
	./bench/examples "abc"       > test/diff/examples-abc.out

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk
