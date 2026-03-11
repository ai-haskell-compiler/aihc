# bzlib [![Hackage][Hackage badge]][Hackage page]

**Compression and decompression in the bzip2 format**

This package provides a pure interface for compressing and decompressing streams of data represented as lazy `ByteString`s. It uses the `bz2` C library so it has high performance.

It provides a convenient high level API suitable for most tasks and for the few cases where more control is needed it provides access to the full `bzip2` feature set.

[Hackage page]: https://hackage.haskell.org/package/bzlib
[Hackage badge]: https://img.shields.io/hackage/v/bzlib.svg
