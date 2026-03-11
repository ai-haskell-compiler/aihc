TYPES = Float Double ComplexFloat ComplexDouble
TEST_MODULES = test/Test/Numeric/BLAS/Slice.hs \
  $(patsubst %, test/Test/%/Main.hs, $(TYPES))
TESTABLE = \
  src/Numeric/BLAS/Vector.hs \
  src/Numeric/BLAS/Vector/Slice.hs \
  src/Numeric/BLAS/Matrix/RowMajor.hs


run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test comfort-blas-test --show-details=streaming

update-test:	$(TEST_MODULES)

test/Test/Numeric/BLAS/Slice.hs: src/Numeric/BLAS/Slice.hs
	doctest-extract-0.1 -i src/ -o test/ --module-prefix Test Numeric.BLAS.Slice

test/Test/%/Main.hs:	test-module.list $(TESTABLE)
	doctest-extract-0.1 -i src/ -o test/ --module-prefix Test.$* --library-main Main $$(cat test-module.list)
	for file in $$(find test/Test/$*/Numeric -name "*.hs"); \
	  do perl -i -p -e "s:NumberModule:$*:" $$file; \
	done

test-module.list:       comfort-blas.cabal
	grep '^ \+Test\.Float\.Numeric\.BLAS\.' $< | cut -d. -f3- >$@
