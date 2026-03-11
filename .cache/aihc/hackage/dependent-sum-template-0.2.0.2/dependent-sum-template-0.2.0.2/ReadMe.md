dependent-sum-template [![Build Status](https://travis-ci.org/obsidiansystems/dependent-sum-template.svg)](https://travis-ci.org/obsidiansystems/dependent-sum-template) [![Hackage](https://img.shields.io/hackage/v/dependent-sum-template.svg)](http://hackage.haskell.org/package/dependent-sum-template)
==============

This library defines [Template Haskell](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/template_haskell.html) functions for deriving the `GEq`, `GCompare`, `GShow`, and `GRead` functions from the [`some`](https://hackage.haskell.org/package/some) library.

 - `GEq tag` is similar to an `Eq` instance for `tag a` except that with `geq`, values of types `tag a` and `tag b` may be compared, and in the case of equality, evidence that the types `a` and `b` are equal is provided.

 - `GCompare tag` is similar to the above for `Ord`, and provides `gcompare`, giving a `GOrdering` that gives similar evidence of type equality when values match.

 - `GShow tag` means that `tag a` has (the equivalent of) a `Show` instance.

 - `GRead tag` means that `tag a` has (the equivalent of) a `Read` instance.
