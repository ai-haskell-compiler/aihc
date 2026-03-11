# Welcome to snappy

snappy is a fast Haskell library for working with data compressed
using Google’s Snappy format. It is implemented as a binding to the
[Snappy library](http://github.com/google/snappy).

It implements zero-copy compression and decompression of both strict
and lazy [bytestrings](http://hackage.haskell.org/package/bytestring),
the standard Haskell types for managing binary data efficiently.

# Join in!

We are happy to receive bug reports, fixes, documentation enhancements,
and other improvements.

Please report bugs via the
[Framagit issue tracker](http://framagit.org/ljdarj/snappy/-/issues).

Master [git repository](http://framagit.org/ljdarj/snappy):

* `git clone https://framagit.org/ljdarj/snappy.git`

Authors
-------

This library was written by Bryan O'Sullivan (<bos@serpentine.com>) and
is maintained by ARJANEN Loïc Jean David (<ljd@luigiscorner.mu>).
