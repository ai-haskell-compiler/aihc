FLATPAK = $$FLATPAKBUILD

all:	ReadMe.html

%.html: %.md
	pandoc --standalone --metadata pagetitle="$*" --output=$@ $<


ghci:
	ghci -Wall -i:src src/Main.hs

testbuild:
	runhaskell Setup configure --user
	runhaskell Setup build
	runhaskell Setup haddock

%.flv:	%.flac
	ffmpeg -r 6 -i /suse10/sound-collage/market/%06d.jpeg \
	   -i $< -t 00:03:03 -b:a 384k -b:v 4000k $@

# dist-newstyle/cache/plan.json:
#	cabal new-build --dry-run

flatpak.json:	flatpak.cabal.json dist-newstyle/cache/plan.json
	cabal-flatpak --cabal-install --arch x86_64 --arch i386 $< $@

repo-%:	flatpak.json
	flatpak-builder --jobs=4 --force-clean --arch=$* --repo=$(FLATPAK)/repository --state-dir=$(FLATPAK)/builder/ $(FLATPAK)/build/sound-collage $<
	touch $@

sound-collage.%.flatpak:	repo-%
	flatpak build-bundle --arch=$* $(FLATPAK)/repository $@ com.github.thielema.sound-collage \
	  --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo

flatpak-all:	sound-collage.x86_64.flatpak sound-collage.i386.flatpak
