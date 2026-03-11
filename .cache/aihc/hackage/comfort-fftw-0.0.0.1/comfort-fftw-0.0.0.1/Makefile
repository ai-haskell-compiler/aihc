run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/fftw-test/fftw-test

update-test:
	doctest-extract-0.1 -i src/ -o test/ --library-main=Main $$(cat test-module.list)
