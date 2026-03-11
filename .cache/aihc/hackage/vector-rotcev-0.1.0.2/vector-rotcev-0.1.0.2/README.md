# vector-rotcev [![Hackage](http://img.shields.io/hackage/v/vector-rotcev.svg)](https://hackage.haskell.org/package/vector-rotcev) [![Stackage LTS](http://stackage.org/package/vector-rotcev/badge/lts)](http://stackage.org/lts/package/vector-rotcev) [![Stackage Nightly](http://stackage.org/package/vector-rotcev/badge/nightly)](http://stackage.org/nightly/package/vector-rotcev)

A wrapper for an arbitrary `Vector` with O(1) `reverse`. Instead of creating a copy, it just flips a flag, which inverts indexing. Imagine it as a vector with a switch between little-endianness and big-endianness.

Let us have a vector of data `vec :: Vector Int`, which is manipulated via `Data.Vector.Generic`-based API. You can wrap it into `Forward vec :: Rotcev Vector Int` and leave everything else unchanged, because `Rotcev Vector Int` still has all `Vector` instances. Then apply `reverse (Forward vec) :: Rotcev Vector Int`, which effectively reverses the vector in O(1) time and space. Internally instead of actual reversing, `reverse` just provides a _view_ (or a _lens_) with an inverted order of indexing, which affects all `Data.Vector.Generic` API.

For example,

```haskell
> vec = Data.Vector.Generic.fromList [0..100] :: Data.Vector.Vector Int
> cev = reverse (Forward vec) :: Rotcev Data.Vector.Vector Int
> cev Data.Vector.Generic.! 10
90
```

In a mutable setting you can freely manipulate original and reversed vectors simultaneously, using `mreverse` function:

```haskell
> Control.Monad.ST.runST $ do
    vec <- Data.Vector.Generic.Mutable.replicate 3 0.0
    let cev = mreverse vec
    Data.Vector.Generic.Mutable.write vec 0 1.0
    Data.Vector.Generic.Mutable.write cev 0 2.0
    unRotcev <$> Data.Vector.Generic.freeze vec
    :: Data.Vector.Vector Double
[1.0,0.0,2.0]
```

This library works for any flavor of `Vector`: boxed, unboxed, storable, whatever.
