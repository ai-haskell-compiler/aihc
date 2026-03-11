ghci:	ghci-tmp

ghci-tmp:
	ghci -Wall -i:src:dist/build/autogen:execute/tmp src/Graphics/Gnuplot/Simple.hs

ghci-pipe:
	ghci -Wall -i:src:dist/build/autogen:execute/pipe src/Graphics/Gnuplot/Simple.hs

ghci-shell:
	ghci -Wall -i:src:dist/build/autogen:execute/shell src/Graphics/Gnuplot/Simple.hs

ghci-demo:
	ghci -Wall -i:src:dist/build/autogen:execute/tmp src/Demo.hs

testbuild:
	runhaskell Setup configure --user -f buildExamples
	runhaskell Setup build
	runhaskell Setup haddock

	runhaskell Setup clean
	runhaskell Setup configure --user -f executePipe
	runhaskell Setup build

	runhaskell Setup clean
	runhaskell Setup configure --user -f executeShell
	runhaskell Setup build
