# duration

A tiny compile-time time utility library, inspired by [zeit/ms](https://github.com/zeit/ms).

## Examples

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Time.Clock (DiffTime, NominalDiffTime)
import Data.Time.Clock.Duration (t, s, ms, µs, ns, ps)

main :: IO ()
main = do
    print @DiffTime [t| 1 day |] -- 86400s
    print @DiffTime [t| 2h |]    -- 7200s
    print @DiffTime [t| -1m |]   -- -60s

    print @NominalDiffTime [s| 1 |]  -- 1s
    print @NominalDiffTime [ms| 1 |] -- 0.001s
    print @NominalDiffTime [µs| 1 |] -- 0.000001s
    print @NominalDiffTime [ns| 1 |] -- 0.000000001s
    print @NominalDiffTime [ps| 1 |] -- 0.000000000001s

    print @Int      [s| 2 days |]  -- 172800
    print @Integer  [ms| 5s |]     -- 5000
    print @Double   [µs| 2min |]   -- 1.2e8
    print @Float    [ns| -1 sec |] -- -1.0e9
    print @Rational [ps| 10ms |]   -- 10000000000 % 1
```
