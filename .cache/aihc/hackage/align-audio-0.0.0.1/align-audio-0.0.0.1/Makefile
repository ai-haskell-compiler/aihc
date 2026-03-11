CABALOPTS =

run-test:
	runhaskell Setup configure --user
	runhaskell Setup build
	runhaskell Setup haddock

	runhaskell Setup configure --user -fblas
	runhaskell Setup build

	runhaskell Setup configure --user -flapack
	runhaskell Setup build

align-audio.deb:
	cabal v2-install $(CABALOPTS) \
	  --installdir=bin --overwrite-policy=always --install-method=copy
	fpm -s dir -t deb -n align-audio -v 0.0 -p $@ \
	  -d libsox3 \
	  -d libfftw3-single3 -d libfftw3-double3 \
	  --description='Find relative time displacement of two recordings of the same music' \
	  bin=/usr

align-audio-blas.deb:
	cabal v2-install -fblas $(CABALOPTS) \
	  --installdir=bin --overwrite-policy=always --install-method=copy
	fpm -s dir -t deb -n align-audio -v 0.0 -p $@ \
	  -d libblas3 -d liblapack3 \
	  -d libsox3 \
	  -d libfftw3-single3 -d libfftw3-double3 \
	  --description='Find relative time displacement of two recordings of the same music' \
	  bin=/usr
