.PHONY: clean

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules
.DELETE_ON_ERROR:

setup: valgrind-3.15.0.tar

clean:
	rm -rf dist-newstyle dist *.tar* *.hp *.prof tags *.svg

valgrind-3.15.0.tar.bz2:
	curl https://sourceware.org/pub/valgrind/valgrind-3.15.0.tar.bz2 -o $@

valgrind-3.15.0.tar: valgrind-3.15.0.tar.bz2
	bzip2 -d $^ --keep
