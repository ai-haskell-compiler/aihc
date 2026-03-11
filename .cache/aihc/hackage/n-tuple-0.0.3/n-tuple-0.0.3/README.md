# n-tuple

This is a silly implementation of "homogeneous n-length tuples" -- basically
an array. Internally, it builds a `Vector`, and projections just pull that index.

```haskell
{-# LANGUAGE DataKinds -#}

import Data.NTuple


foo :: NTuple 3 String
foo
  = incl _3 "three"
  . incl _2 "two"
  . incl _1 "one"
  $ empty


one :: String
one = proj _1 foo

two :: String
two = proj _2 foo
```
