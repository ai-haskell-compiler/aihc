# time-units-types

![MIT](https://img.shields.io/github/license/mbg/time-units-types)
![Build](https://github.com/mbg/time-units-types/workflows/build/badge.svg?branch=main)
[![Hackage](https://img.shields.io/hackage/v/time-units-types)](https://hackage.haskell.org/package/time-units-types)

This Haskell library exports types which can be used to describe simple type periods at the type-level. The type-level descriptions can then be reifed to corresponding values of the [`time-units`](https://hackage.haskell.org/package/time-units) library.

```haskell
{-# LANGUAGE TypeApplications #-}

import Data.Time.TypeLevel

type TenSeconds = 'Second 10

main :: IO ()
main = do
    -- `durationVal` converts the type-level specification to a corresponding
    -- value from "Data.Time.Units"
    print $ durationVal @TenSeconds
    -- `durationMicroseconds` converts the type-level specification to a
    -- corresponding integer value, representing microseconds
    print $ durationMicroseconds @TenSeconds
```

See the Haddock documentation for a reference of all supported units.

## See also

Related/similar projects:

- [`time-units`](https://hackage.haskell.org/package/time-units)
- [`o-clock`](https://hackage.haskell.org/package/o-clock)
