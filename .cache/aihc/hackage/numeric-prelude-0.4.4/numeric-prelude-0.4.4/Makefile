HCI6 = ghci
HCI7 = ghci -XCPP -DNoImplicitPrelude=RebindableSyntax

.PHONY: ghci ghci6 ghci7 ghci-gauss ghci-compile

ghci:	ghci7

ghci6:
	$(HCI6) -Wall -i:src:test +RTS -M256m -c30 -RTS test/Demo.hs

ghci7:
	$(HCI7) -Wall -i:src:test +RTS -M256m -c30 -RTS test/Demo.hs

ghci-gauss:
	$(HCI7) -Wall -i:src:test:gaussian +RTS -M256m -c30 -RTS test/Test/MathObj/Gaussian/Variance.hs

ghci-compile:
	$(HCI7) -Wall -i:src:test +RTS -M256m -c30 -RTS -fobject-code -O -hidir=dist/build -odir=dist/build test/Demo.hs


run-test:	update-test
	runhaskell Setup configure --user -fbuildExamples --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/numeric-prelude-test/numeric-prelude-test

update-test:
	doctest-extract-0.1 -i src/ -i gaussian/ -i playground/ -o test/ --executable-main=Test/Run.hs $$(cat test-module.list)


%.html:	%.md
	pandoc $< --output=$@
