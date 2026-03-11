# Makefile for Sit

default :
	cabal v1-install --only-dependencies
	cabal v1-configure
	cabal v1-build
	dist/build/Sit.bin/Sit.bin test/Test.agda

install :
	cabal install
