# Changelog for within

## v0.2.0.0

* Add [path-like](https://hackage.haskell.org/package/path-like) and `PathLike` instance.

## v0.1.0.0

* Change approach to `ComonadEnv` style newtype. `Within` can now store an arbitrary
  inner type whilst maintaining the convenience functions in the special case where
  the inner type is a `Path`.

## v0.0.2.0

* Add `Hashable` and `Ord` instances for `Within`.

## v0.0.1.0

* Add Within Type based on [path](https://hackage.haskell.org/package/path).
Within is a path within another path.
* Add several functions for moving between directories and mapping source
  names.
