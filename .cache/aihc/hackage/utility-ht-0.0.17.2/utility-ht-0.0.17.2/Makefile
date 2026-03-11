ghci:
	ghci -i:src -Wall src/Data/List/HT.hs

jhc:
	jhc -i src --build-hl utility-ht.jhc-cabal

jhc-test:
	jhc -p utility-ht -i src src/Test.hs


run-test:	update-test
	runhaskell Setup configure --user --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test --show-details=streaming

update-test:	test-module.list
	doctest-extract-0.1 -i src/ -o src/ --module-prefix DocTest --executable-main=Test.hs --import-tested $$(cat test-module.list)

test-module.list:	utility-ht.cabal
	grep '^ \+DocTest\.' $< | cut -d. -f2- >$@
