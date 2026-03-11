.SECONDARY:     %.frames

RATE = 50
#RATE = 25
#RATE = 10

RESOLUTION = -dDEVICEHEIGHT=1080 -r108
#RESOLUTION = -dDEVICEHEIGHT=720 -r72
#RESOLUTION = -dDEVICEHEIGHT=540 -r54

%.pdf:	%.mid
	cabal run boomwhacker -- --rate $(RATE) $< $@

%.wav:	%.mid
	timidity --preserve-silence -A300 -Ow $<

# Resolution is one Postscript point (1/72 inch).
%.frames:	%.pdf
	mkdir -p /tmp/$*-frames
	rm -f /tmp/$*-frames/*
	gs -sDEVICE=png16m -sOutputFile=/tmp/$*-frames/%04d.png \
	  -dNOPAUSE -dBATCH -dTextAlphaBits=2 -dGraphicsAlphaBits=2 \
	  $(RESOLUTION) $<
	touch $@

%.flv:	%.frames %.wav
	ffmpeg -r $(RATE) -f image2 -i /tmp/$*-frames/%04d.png -i $*.wav \
	  -vcodec flashsv -acodec copy -y $@

Roboter.flv:	Roboter.frames Roboter.aac
	ffmpeg -r $(RATE) -f image2 -i /tmp/Roboter-frames/%04d.png -i Roboter.aac \
	  -vcodec flashsv -acodec copy -y $@

README.html:	README.md
	pandoc --standalone $< --output $@
