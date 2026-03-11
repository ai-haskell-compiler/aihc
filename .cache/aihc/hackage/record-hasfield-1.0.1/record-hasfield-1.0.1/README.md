# record-hasfield [![Hackage version](https://img.shields.io/hackage/v/record-hasfield.svg?label=Hackage)](https://hackage.haskell.org/package/record-hasfield) [![Stackage version](https://www.stackage.org/package/record-hasfield/badge/nightly?label=Stackage)](https://www.stackage.org/package/record-hasfield) [![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/record-hasfield/ci.yml?branch=master)](https://github.com/ndmitchell/record-hasfield/actions)

This package provides a version of "GHC.Records" as it will be after the implementation of
[GHC proposal #42](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-record-set-field.rst), which reads:

```haskell
-- | Constraint representing the fact that the field @x@ can be get and set on
--   the record type @r@ and has field type @a@.
class HasField x r a | x r -> a where
    -- | Function to get and set a field in a record.
    hasField :: r -> (a -> r, a)
```

In GHC these will be magically solved, but this package doesn't provide that. This package does provide extra helper functions for working with the `HasField` type class.
