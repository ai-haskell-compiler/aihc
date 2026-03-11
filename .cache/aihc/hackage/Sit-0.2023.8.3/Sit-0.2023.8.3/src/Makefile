.PHONY: test
.PRECIOUS : %.hs %.x %.y
all : test

test : Sit.bin ../test/Test.agda
	Sit.bin ../test/Test.agda

loop : Sit.bin ../test/Loop.agda
	Sit.bin ../test/Loop.agda

Sit.bin : Sit.hs Sit/Abs.hs Sit/Lex.hs Sit/Par.hs \
  undefined.h Impossible.hs \
  Abstract.hs Internal.hs Substitute.hs Evaluation.hs TypeChecker.hs
	ghc --make $< -o Sit.bin

Sit/Lex.x Sit/Par.y Sit/Abs.hs : Sit.cf
	bnfc --haskell -d $<
	touch $@

Sit/Par.hs: Sit/Par.y
	happy -gcai $<

Sit/Lex.hs: Sit/Lex.x
	alex -g $<

Sit/Test: Sit/Test.hs Sit/Par.hs Sit/Lex.hs
	ghc --make $< -o $@

clean:
	-rm -f Sit/*.log Sit/*.aux Sit/*.hi Sit/*.o Sit/*.dvi

distclean: clean
	-rm -f Sit/Doc.* Sit/Lex.* Sit/Par.* Sit/Layout.* Sit/Skel.* Sit/Print.* Sit/Test.* Sit/Abs.* Sit/Test Sit/ErrM.* Sit/SharedString.* Sit/ComposOp.* Sit/Sit.dtd Sit/XML.*
		-rmdir -p Sit/
