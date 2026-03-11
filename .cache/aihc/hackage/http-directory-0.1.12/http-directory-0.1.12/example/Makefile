all: latest-ghc list-page

latest-ghc: latest-ghc.hs
	ghc latest-ghc
	./latest-ghc

list-page: list-page.hs
	ghc list-page
	./list-page https://download.fedoraproject.org/pub/fedora/linux
clean:
	rm -f *.o *.hi latest-ghc list-page
