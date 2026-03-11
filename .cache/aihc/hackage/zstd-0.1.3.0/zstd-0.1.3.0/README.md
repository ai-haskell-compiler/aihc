# Zstandard bindings for Haskell

[![Travis](https://api.travis-ci.com/luispedro/hs-zstd.png)](https://travis-ci.com/luispedro/hs-zstd)

This library provides Haskell bindings to the
[Zstandard compression library](http://facebook.github.io/zstd/).

Note that is now the official repositoryc for the
[zstd](https://hackage.haskell.org/package/zstd) Haskell package. The original
authors (Facebook) are no longer interested in maintaining it.

The library is structured to provide several layers of abstraction.

* For the simplest use cases, the top-level
  [`Zstd`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd.html)
  module is the best place to get started.

* If you need to stream a large amount of data with a constant memory
  footprint, use the
  [`Zstd.Streaming`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd-Streaming.html)
  module. See also the
  [`conduit-zstd`](https://hackage.haskell.org/package/conduit-zstd) package
  which provides a very thin wrapper to integrate with the `conduit` library.
  If you need to use lazy bytestrings instead, see the
  [`Zstd.Lazy`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd-Lazy.html)
  module. This is built using the abstractions from the `Zstd.Streaming`
  module.

* When your usage is dominated by lots of small messages (presumably
  using pre-computed compression dictionaries), use the
  [`Zstd.Efficient`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd-Efficient.html)
  module to amortize the cost of allocating and initializing context
  and dictionary values.

## Join in

If you'd like to help improve the code, please
[read the contribution guidelines](CONTRIBUTING.md).  This discusses
how to file bugs and submit changes to the code itself.

## API documentation

The APIs should be easy to understand and work with, and you can find
[documentation on Hackage](http://hackage.haskell.org/package/zstd).
