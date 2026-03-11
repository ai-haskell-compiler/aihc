#!/bin/sh

(./from_dtd.pl data/*.dtd; ./from_HTML-Tagset.pl; ./from_xsd.hs) \
	| sort | uniq
