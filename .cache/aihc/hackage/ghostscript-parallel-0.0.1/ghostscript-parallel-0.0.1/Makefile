.SECONDARY:     %.frames

CABALOPTS =

RESOLUTION = 108
#RESOLUTION = 72
#RESOLUTION = 54

PAGES = --last-page=1000
#PAGES = --first-page=100 --last-page=200

ANTIALIAS = -dTextAlphaBits=2 -dGraphicsAlphaBits=2

%.frames:	%.pdf	src/Main.hs
	mkdir -p /tmp/$*-frames
	rm -f /tmp/$*-frames/*
	cabal run gs-parallel $(CABALOPTS) -- \
	  -j4 $(PAGES) $< "/tmp/$*-frames/%04d.png" -- \
	  -dNOPAUSE -dBATCH \
	  -sDEVICE=png16m $(ANTIALIAS) -r$(RESOLUTION)
	touch $@

# although I request 4 pages and ghostscript claims,
# that it processed 4 pages, it emits only one.
# Also happens when rendering the PDF-1.5 reference.
# It seems that PageList option is seriously broken in gs version 9.53.3 (2020-10-01).
# Seems to be fixed in version 10.01.2 (2023-06-21), though.
%0001.png:	%.pdf	src/Main.hs
	gs -dNOPAUSE -dBATCH \
	  -sDEVICE=png16m $(ANTIALIAS) \
	  -dDEVICEHEIGHT=$(RESOLUTION)0 -r$(RESOLUTION) \
	  -sOutputFile=$*%04d.png -sPageList=1,5,9,13 $<
#	  -sOutputFile=$*%04d.png -dFirstPage=1000 -dLastPage=1100 $<
