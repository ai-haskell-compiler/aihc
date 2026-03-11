# bz2

High-level bindings to [bzip2](https://www.sourceware.org/bzip2/).

## Comparison

Compared to [bzlib](http://hackage.haskell.org/package/bzlib):

  * Bundles `bzip2` 1.0.8.
  * Compatible with GHC 8.8+
  * Uses [c2hs](https://github.com/haskell/c2hs) rather than
    [hsc2hs](https://hackage.haskell.org/package/hsc2hs) and thus has a larger
    dependency footprint

Unlike [pipes-bzip](http://hackage.haskell.org/package/pipes-bzip) and
[bzlib-conduit](http://hackage.haskell.org/package/bzlib-conduit), bz2 can be
cross-compiled.

## Backpack Integration

`bz2` implements
[`bzip-signature`](http://hackage.haskell.org/package/bzip-signature), which
makes it interchangeable with `bzlib`.
