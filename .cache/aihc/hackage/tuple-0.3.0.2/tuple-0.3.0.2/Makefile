all:
	@echo No target

N=32

update:	updateCurry updateSelect updateSequence updateUpd

updateCurry:
	make N=$(N) UPDFILE=src/Data/Tuple/Curry.hs UPD=curry updateIt

updateSelect:
	make N=$(N) UPDFILE=src/Data/Tuple/Select.hs UPD=select updateIt

updateSequence:
	make N=$(N) UPDFILE=src/Data/Tuple/Sequence.hs UPD=sequence updateIt

updateUpd:
	make N=$(N) UPDFILE=src/Data/Tuple/Update.hs UPD=update updateIt

updateIt:
	sed '/--snip--/,$$d' $(UPDFILE) > tmp.hs
	runhaskell Tools/MkTuple $(UPD) $(N) >> tmp.hs
	mv tmp.hs $(UPDFILE)

install:
	cabal install
