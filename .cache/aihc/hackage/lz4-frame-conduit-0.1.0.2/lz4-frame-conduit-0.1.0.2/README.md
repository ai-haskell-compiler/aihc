# lz4-frame-conduit

Haskell Conduit implementing the official LZ4 frame streaming format.

## Building

Use `git clone --recursive`, because `lz4/` is a git submodule.

## Rationale and comparison to non-`lz4`-compatible libraries

There exist two `lz4` formats:

1. the **block format**, limited to compressing data < 2 GB
2. the **frame format**, suitable for streaming arbitrarily sized data
  ** This is what the `lz4` command line utility uses

**This library implements the frame format.**

Some existing Haskell libraries implement only the block format, and are thus not suitable to compress data > 2 GB in a way.
(Of course they could chunk the output in some arbitrary way, but that wouldn't be compatible with the `lz4` command line utility.)
The libraries that belong to this category at time of writing are:

* [`lz4`](https://hackage.haskell.org/package/lz4)
* [`lz4-conduit`](https://hackage.haskell.org/package/lz4-conduit)
