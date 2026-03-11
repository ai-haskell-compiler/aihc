FLATPAK = $$FLATPAKBUILD

ghci:
	ghci -Wall -i:src src/Game/Mastermind.hs

run-test:	update-test
	runhaskell Setup configure --user \
	  --enable-tests --enable-benchmarks -fbuildExamples
	runhaskell Setup build
	runhaskell Setup haddock
	./dist/build/board-games-test/board-games-test

update-test:
	doctest-extract-0.1 -i src/ -i private/ -o test/ --executable-main=Test.hs $$(cat test-module.list)


criterion.html:	./dist/build/board-games-benchmark/board-games-benchmark
	$< --output=$@ $(patsubst %.html, %.csv, --csv=$@)


dist-newstyle/cache/plan.json:
	cabal new-build --dry-run --disable-benchmarks --disable-tests

flatpak.json:	flatpak.cabal.json dist-newstyle/cache/plan.json
	cabal-flatpak --cabal-install --arch x86_64 --arch i386 $< $@

repo-%:	flatpak.json
	flatpak-builder --force-clean --arch=$* --repo=$(FLATPAK)/repository \
	  --state-dir=$(FLATPAK)/builder/ $(FLATPAK)/build/games $<
	touch $@

board-games.%.flatpak:	repo-%
	flatpak build-bundle --arch=$* $(FLATPAK)/repository $@ com.github.thielema.board-games \
	  --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo

flatpak-all:	board-games.x86_64.flatpak board-games.i386.flatpak
