.SECONDARY:	%.frames

%.pdf:	%.mmd?
	cabal run animed -- --frame-pattern="/tmp/page-%04d.png" $<

%.wav:	%.mmd1
	uade123 --panning=0.5 -f $@ $<

# Resolution is one Postscript point (1/72 inch).
%.frames:	%.pdf
	export width=$$(fgrep '%%DocumentMedia:' $< | head -n1 | cut -f3 -d' '); \
	gs -sDEVICE=pnggray -sOutputFile=/tmp/page-%04d.png \
	  -dNOPAUSE -dBATCH -dTextAlphaBits=2 -dGraphicsAlphaBits=2 \
	  -g"$$width"x720 -r72 $<
	touch $@

%.flv:	%.frames %.wav
	ffmpeg -f concat -safe 0 -i $*.cue -i $*.wav \
	  -vcodec flashsv -acodec copy -y $@

README.html:	README.md
	pandoc $< --output $@
