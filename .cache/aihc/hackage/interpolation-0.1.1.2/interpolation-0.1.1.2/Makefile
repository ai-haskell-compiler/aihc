run-test:	update-test
	runhaskell Setup configure --user --enable-test -fbuildExamples -flapack
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test --show-details=always

update-test:
	doctest-extract -i src/ -o test/ --module-prefix Test --executable-main=Test.hs $$(cat test-module.list)
