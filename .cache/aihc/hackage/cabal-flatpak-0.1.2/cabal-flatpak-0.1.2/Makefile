FLATPAK = $$FLATPAKBUILD

%.html:	%.md
	pandoc --output $@ $<

dist-newstyle/cache/plan.json:
	cabal new-build --dry-run

flatpak.json:	flatpak.cabal.json dist-newstyle/cache/plan.json
	cabal run -- --cabal-install --arch x86_64 --arch i386 $< $@

repo-%:	flatpak.json
	flatpak-builder --force-clean --arch=$* --repo=$(FLATPAK)/repository --state-dir=$(FLATPAK)/builder/ $(FLATPAK)/build/cabal-flatpak $<
	touch $@

cabal-flatpak.%.flatpak:	repo-%
	flatpak build-bundle --arch=$* $(FLATPAK)/repository $@ com.github.thielema.cabal-flatpak \
	  --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo

flatpak-all:	cabal-flatpak.x86_64.flatpak cabal-flatpak.i386.flatpak
