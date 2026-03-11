run-test:	update-test
	runhaskell Setup configure --user -fbuildExamples --enable-tests
	runhaskell Setup build
	runhaskell Setup haddock
	runhaskell Setup test spreadsheet-test --show-details=streaming

update-test:
	doctest-extract-0.1 -i src/ -o test/ --executable-main=Main.hs \
	  Data.Spreadsheet


ghci:
	ghci -Wall -XGeneralizedNewtypeDeriving -i:src src/Data/Spreadsheet.hs

%.html:	%.md
	pandoc --output $@ $<
