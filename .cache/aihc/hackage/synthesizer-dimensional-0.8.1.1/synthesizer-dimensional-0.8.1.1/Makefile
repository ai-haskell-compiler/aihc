# HIDE_SYNTH = -hide-package synthesizer

ghci:
	ghci -i:src -Wall $(HIDE_SYNTH)

ghci-comp:
	ghci -Wall -fobject-code -fexcess-precision -O2 -fvia-C -optc-O2 \
	   -odirdist/build -hidirdist/build $(HIDE_SYNTH) -i:src src/Synthesizer/Dimensional/RateAmplitude/Rain.hs
