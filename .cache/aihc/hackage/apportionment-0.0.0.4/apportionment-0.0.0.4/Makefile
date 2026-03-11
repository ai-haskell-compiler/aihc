run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test apportionment-test --show-details=streaming

update-test:
	doctest-extract-0.1 -i src/ -o test/ --import-tested --executable-main=Main.hs Math.Apportionment
