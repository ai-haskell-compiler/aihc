run-test:
	runhaskell Setup configure --user -fbuildExamples --enable-tests
	runhaskell Setup build
	runhaskell Setup configure --user -fbuildExamples -fllvm --enable-tests --enable-benchmarks
	runhaskell Setup build
	runhaskell Setup haddock
	dist/build/set-cover-test/set-cover-test
# more portable, but suppresses live QuickCheck test counter:
#	runhaskell Setup test --show-details=streaming
