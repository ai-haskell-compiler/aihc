run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test comfort-graph-test --show-details=streaming

update-test:
	doctest-extract-0.1 -i src/ -o test/ --executable-main=Main.hs Data.Graph.Comfort
