This Haskell package provides a
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) data
generator and shrink functions for testing software that uses Unicode
data.

The default
[`Arbitrary`](http://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck.html#t:Arbitrary)
instance for the `Char` type *intentionally* generates *only* ASCII
values.  This can lead to a false sense of security in cases where
Unicode compliance is required, as encodings that span multiple bytes
or code units will simply not be exercised at all.

This module deliberately avoids using the `text` and `bytestring`
packages to avoid pulling in extra dependencies.
