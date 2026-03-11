# Fast compression for Haskell ByteStrings

This library implements Haskell bindings to
[QuickLZ](http://quicklz.com), a fast compression library. The
interface is very simple:

```haskell
compress    :: ByteString -> ByteString
decompress  :: ByteString -> ByteString
decompress' :: ByteString -> ByteString -- overlapping decompression
```

As the name states, QuickLZ is fast - at both compression and
decompression.  Per their own benchmarks, at compression level 1,
QuickLZ 1.5.0 has a compression speed of 308Mbyte/s, and a
decompression speed of 358Mbyte/s (their benchmarks, Core i7 920.)

[travis-ci.org](http://travis-ci.org) results: [![Build Status](https://secure.travis-ci.org/thoughtpolice/hs-quicklz.png?branch=master)](http://travis-ci.org/thoughtpolice/hs-quicklz)

# Installation

Just use cabal:

```
cabal install quicklz
```

# Join in

File bugs in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-quicklz.git`

# Authors

See `AUTHORS.txt`.

# License.

GPLv2. See `LICENSE.txt` for terms of copyright and redistribution.

# TODO

This package has some benchmarks and tests associated - I would like
to expand the benchmarks to include bigger data sets in the future.

There is currently only an interface for strict `ByteString`s.

I would like to implement the streaming mode functionality sometime in
the future, perhaps tying it to John Millikin's
[enumerator](http://hackage.haskell.org/package/enumerator) package.

[issue tracker]: https://github.com/thoughtpolice/hs-quicklz/issues
[gh]: https://github.com/thoughtpolice/hs-quicklz
