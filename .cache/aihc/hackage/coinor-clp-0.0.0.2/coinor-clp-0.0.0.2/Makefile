run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test coinor-clp-test --show-details=streaming

	runhaskell Setup configure --user -fdebug
	runhaskell Setup build

update-test:
	doctest-extract-0.1 -i src/ -o test/ --executable-main=Main.hs \
	  Numeric.COINOR.CLP Numeric.COINOR.CLP.Monad
