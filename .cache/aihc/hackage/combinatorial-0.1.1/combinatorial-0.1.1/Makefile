run-test:	update-test
	runhaskell Setup configure --user --enable-test
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test --show-details=streaming

update-test:
	doctest-extract-0.1 -i src/ -o test/ --module-prefix Test --executable-main=Test.hs $$(cat test-module.list)
