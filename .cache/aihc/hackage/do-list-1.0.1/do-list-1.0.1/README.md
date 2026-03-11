# do-list [![do-list][stackage-badge]][do-list] [![do-list][hackage-badge]][hackage]
Do notation for free.

## Summary
[do-list] makes it easy to use do notation. You can construct lists or monoids using [`DoList`] or [`DoMonoid`] modules respectively. [do-list] is designed to work well with `OverloadedStrings` and `OverloadedLists` extensions. See examples.

As alternative there is a canonical [`Writer`] without overloading support.

## Examples

### Benchmarks
```haskell
{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Criterion.Main
import Data.DoList (DoList, item, toList)

main :: IO ()
main = defaultMain $ toList $ do
  doBench "add"  $ whnf (2 +) (1 :: Int)
  doBench "sub" $ whnf (2 -) (1 :: Int)
  -- Regular criterion benchmarks are injected via list overloading
  [multBench, divBench]

multBench, divBench :: Benchmark
multBench = bench "mult" $ whnf (2 *) (2 :: Int)
divBench = bench "div" $ whnf (2 `div`) (2 :: Int)

-- Now we can define benchmarks with do notation.
doBench :: String -> Benchmarkable -> DoList Benchmark
doBench name = item . bench name
```

### Multiline Text
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Data.DoMonoid (runDoM)
import Data.Text.IO as T (putStr)

main :: IO ()
main = T.putStr $ runDoM $ do
  -- Lines are combined using Text.append
  "fib 0 = 0\n"
  "fib 1 = 1\n"
  "fib n = fib (n-1) + fib (n-2)\n"
```

### Indentation
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Data.DoList (DoList, fromList, toList)
import Data.Text as T (Text, append, unlines)
import Data.Text.IO as T (putStr)

main :: IO ()
main = T.putStr $ T.unlines $ toList $ do
  "Here goes indented list:"
  indent $ do
    "1. Item 1"
    indent $ do
      "* Item a"
      "* Item b"
    "2. Item 2"
    "3. Item 3"

indent :: DoList Text -> DoList Text
-- fromList and toList are no-ops
indent = fromList . map (append "  ") . toList
```

[do-list]: https://www.stackage.org/package/do-list
[hackage]: https://hackage.haskell.org/package/do-list
[hackage-badge]: https://img.shields.io/hackage/v/do-list.svg
[stackage-badge]: https://www.stackage.org/package/do-list/badge/lts
[`DoList`]: https://hackage.haskell.org/package/do-list/docs/Data-DoList.html
[`DoMonoid`]: https://hackage.haskell.org/package/do-list/docs/Data-DoMonoid.html
[`Writer`]: https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Writer-Lazy.html
