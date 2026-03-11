FAY:="fay"

fay-demo.js: fay-demo.hs Optimisation/CirclePacking.hs
	$(FAY) fay-demo.hs
	
Optimisation/CirclePacking.hs: ../Optimisation/CirclePacking.hs
	cpphs --noline $? -O$@

