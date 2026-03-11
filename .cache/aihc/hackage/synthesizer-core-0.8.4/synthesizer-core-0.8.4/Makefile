MODULE_PATH = src:src-4

# HIDE_SYNTH = -hide-package synthesizer


ghci:
	ghci -Wall -odirdist/build -hidirdist/build $(HIDE_SYNTH) -i:$(MODULE_PATH)

ghci7:
	ghci -Wall -odirdist/build -hidirdist/build $(HIDE_SYNTH) -i:$(MODULE_PATH) -XCPP -DNoImplicitPrelude=RebindableSyntax

tutorial:
	ghci -Wall -fobject-code -fexcess-precision -O2 -fvia-C -optc-O2 -odirdist/build -hidirdist/build $(HIDE_SYNTH) -i:$(MODULE_PATH) src/Synthesizer/Generic/Tutorial.hs

# test new haddock features
haddock:
	haddock -h -o dist/doc/html/test/ src/Synthesizer/Plain/Signal.hs src/Synthesizer/Plain/Cut.hs
