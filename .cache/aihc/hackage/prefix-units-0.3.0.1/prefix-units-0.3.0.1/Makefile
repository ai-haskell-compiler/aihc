CABAL_OPTS = --hyperlink-source

build:
	cabal build

int-docs:
	cabal haddock \
	  $(CABAL_OPTS) \
	  --internal

haddock:
	cabal haddock \
	  $(CABAL_OPTS)

configure:
	cabal clean
	cabal configure --enable-tests --enable-library-coverage

test:
	cabal build
	cabal test

dist:
	cabal sdist

lint:
	@rm -f report.html
	hlint \
	  --cpp-define='MIN_VERSION_QuickCheck(a,b,c)=1' \
	  --cpp-define='MIN_VERSION_base(a,b,c)=1' \
	  --report -c Data tests

clean:
	cabal clean
	rm -rf dist
	rm -f report.html

.PHONY: build int-docs haddock configure test dist lint clean
