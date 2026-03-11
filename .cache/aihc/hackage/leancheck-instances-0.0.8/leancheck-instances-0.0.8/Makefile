# Makefile for leancheck-instances
#
# Copyright:   (c) 2015-2022 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = test/main      \
        test/text
EGS = \
	eg/test-text
BENCHS = \
	bench/tiers
GHCIMPORTDIRS = src:test
# -dynamic is needed only for src/Test/LeanCheck/Derive.hs and test/derive.hs
GHCFLAGS = -v0 -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HADDOCKFLAGS = \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic)
BASE_DEPS = containers array time
INSTALL_DEPS = leancheck bytestring nats text \
  $(shell cabal --version | grep -q "version [0-2]\." || echo $(BASE_DEPS))

all: mk/toplibs

all-all: mk/All.o

test: $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

diff-test: diff-test-tiers $(patsubst %,%.diff-test,$(EGS))

update-diff-test: update-diff-test-tiers $(patsubst %,%.update-diff-test,$(EGS))

%.run: %
	./$<

eg/%.diff-test: eg/%
	./$< | diff -rud test/diff/$<.out -

eg/%.update-diff-test: eg/%
	./$< >           test/diff/$<.out

clean: clean-hi-o clean-haddock
	rm -f $(TESTS) $(BENCHS) $(EGS) mk/toplibs

full-clean: clean
	rm -rf .stack-work
	rm -f tags TAGS

ghci: mk/All.ghci

install:
	@echo "use \`cabal install' instead"

test-sdist:
	./test/sdist

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test main

test-via-stack:
	stack test leancheck-instances:test:main --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

test-with-ghc-9.12:
	make test GHC=ghc-9.12 GHCIMPORTDIRS=src:test:../leancheck/src

test-with-ghc-9.10:
	make test GHC=ghc-9.10 GHCIMPORTDIRS=src:test:../leancheck/src

legacy-test: # needs ghc-8.2 .. ghc-7.8 installed as such
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

hlint:
	hlint \
	  --ignore "Use import/export shortcut" \
	  --ignore "Redundant bracket" \
	  .

markdown:
	pandoc README.md -o README.html

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk

diff-test-tiers: bench/tiers
	./bench/tiers "Natural"          | diff -rud test/diff/tiers-Natural.out     -
	./bench/tiers "Text"             | diff -rud test/diff/tiers-Text.out        -
	./bench/tiers "ByteString" 6     | diff -rud test/diff/tiers-ByteString.out  -
	./bench/tiers "Set Int"          | diff -rud test/diff/tiers-SetInt.out      -
	./bench/tiers "Set Bool"         | diff -rud test/diff/tiers-SetBool.out     -
	./bench/tiers "Map Int Int"      | diff -rud test/diff/tiers-MapIntInt.out   -
	./bench/tiers "Map Bool Bool"    | diff -rud test/diff/tiers-MapBoolBool.out -
	./bench/tiers "Seq Int"          | diff -rud test/diff/tiers-SeqInt.out      -
	./bench/tiers "Seq Bool"         | diff -rud test/diff/tiers-SeqBool.out     -
	./bench/tiers "IntMap Int"       | diff -rud test/diff/tiers-IntMapInt.out   -
	./bench/tiers "IntSet"           | diff -rud test/diff/tiers-IntSet.out      -
	./bench/tiers "UTCTime"        6 | diff -rud test/diff/tiers-UTCTime.out     -
	./bench/tiers "Day"            9 | diff -rud test/diff/tiers-Day.out         -
	./bench/tiers "DiffTime"         | diff -rud test/diff/tiers-DiffTime.out    -
	./bench/tiers "Array Int Int"    | diff -rud test/diff/tiers-ArrayIntInt.out -

update-diff-test-tiers: bench/tiers
	./bench/tiers "Natural"          > test/diff/tiers-Natural.out
	./bench/tiers "Text"             > test/diff/tiers-Text.out
	./bench/tiers "ByteString" 6     > test/diff/tiers-ByteString.out
	./bench/tiers "Set Int"          > test/diff/tiers-SetInt.out
	./bench/tiers "Set Bool"         > test/diff/tiers-SetBool.out
	./bench/tiers "Map Int Int"      > test/diff/tiers-MapIntInt.out
	./bench/tiers "Map Bool Bool"    > test/diff/tiers-MapBoolBool.out
	./bench/tiers "Seq Int"          > test/diff/tiers-SeqInt.out
	./bench/tiers "Seq Bool"         > test/diff/tiers-SeqBool.out
	./bench/tiers "IntMap Int"       > test/diff/tiers-IntMapInt.out
	./bench/tiers "IntSet"           > test/diff/tiers-IntSet.out
	./bench/tiers "UTCTime"        6 > test/diff/tiers-UTCTime.out
	./bench/tiers "Day"            9 > test/diff/tiers-Day.out
	./bench/tiers "DiffTime"         > test/diff/tiers-DiffTime.out
	./bench/tiers "Array Int Int"    > test/diff/tiers-ArrayIntInt.out
