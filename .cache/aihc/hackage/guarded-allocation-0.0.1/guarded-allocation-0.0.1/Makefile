test-build:
	runhaskell Setup configure --user -f-debug
	runhaskell Setup build
	runhaskell Setup haddock

	runhaskell Setup configure --user -fdebug
	runhaskell Setup build

%.html:	%.md
	pandoc $< --output=$@
