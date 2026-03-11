# quickcheck-text

The usual Arbitrary instance for Text (in
[quickcheck-instances](https://hackage.haskell.org/package/quickcheck-instances))
only has single-byte instances and so isn't an ideal representation of
a valid UTF-8 character. This package has generators for one-, two-
and three-byte UTF-8 characters (all that are currently in use).
