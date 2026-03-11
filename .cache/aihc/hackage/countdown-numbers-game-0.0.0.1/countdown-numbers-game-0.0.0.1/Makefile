run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup test countdown-numbers-test --show-details=streaming

update-test:
	doctest-extract-0.1 -i src/ -o test/ --executable-main=TestMain.hs Solve
