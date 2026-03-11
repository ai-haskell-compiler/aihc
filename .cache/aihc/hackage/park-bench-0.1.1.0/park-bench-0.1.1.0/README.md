[![GitHub CI](https://github.com/awkward-squad/park-bench/workflows/Haskell-CI/badge.svg)](https://github.com/awkward-squad/park-bench/actions?query=workflow%3AHaskell-CI) |
[![Hackage](https://img.shields.io/hackage/v/park-bench.svg?label=park-bench&logo=haskell)](https://hackage.haskell.org/package/park-bench)
[![Stackage LTS](https://stackage.org/package/park-bench/badge/lts)](https://www.stackage.org/lts/package/park-bench)
[![Stackage Nightly](https://stackage.org/package/park-bench/badge/nightly)](https://www.stackage.org/nightly/package/park-bench)
[![Dependencies](https://img.shields.io/hackage-deps/v/park-bench)](https://packdeps.haskellers.com/reverse/park-bench)

# Overview

`park-bench` is a quick-and-dirty benchmarking tool for comparing the performance of Haskell functions. Specifically, it
is designed to optimize the workflow in which a programmer makes a small change to a function and wants to measure its
performance impact with as little friction as possible.

![Screenshot](https://github.com/awkward-squad/park-bench/blob/main/images/screenshot.png?raw=true)

# Configuration

| Environment variable name | Type | Meaning | Default value |
| --- | --- | --- | --- |
| `PARK_BENCH_RUNLEN` | Float | The target number of seconds that each benchmark run takes | `0.1` |

# Example usage

Say I am interested in improving the performance of `fib`, which is a function defined in module `MyMathUtilities.Fib`
in a local package called `my-math-utilities`.

## Step 1: Write the function you'd like to benchmark

First, I'm going to copy the implementation of `fib` to a new top-level definition called `fastfib`, tweak its
implementation, and export both from module `MyMathUtilities.Fib`.

If `fib` was private before, that's ok. We only need to expose it for as long as we are interested in benchmarking.

```haskell
module MyMathUtilities.Fib (fib, fastfib, ...) where
```

## Step 2: Write a standalone `bench/Main.hs` module with a `main` function

Next, I'm going to write a standalone `Main.hs` in a subdirectory called `bench`, which will be compiled to an
executable that runs my benchmark.

```haskell
module Main where

-- The module in my local package that I want to benchmark
import MyMathUtilities.Fib

-- This library
import ParkBench

main :: IO ()
main =
  benchmark
    [ function "fib" fib 20
    , function "fastfib" fastfib 20
    ]
```

## Step 3: Define an executable component

Next, I'm going to define an executable component for my benchmark in my `my-math-utilities.cabal` file.

```cabal
executable bench
  build-depends:
    base,
    -- The local package that I want to benchmark
    my-math-utilities,
    -- This library
    park-bench
  default-language: Haskell2010
  ghc-options: -O -rtsopts -with-rtsopts=-T
  hs-source-dirs: bench
  main-is: Main.hs
```

I need to compile the benchmark with `-rtsopts -with-rtsopts=-T`, otherwise my benchmark will not be able to get RTS
statistics from GHC at runtime.

Alternatively, I could compile the benchmark with only `-rtsopts`, but then I'll have to provide `+RTS -T` to the
executable later.

## Step 4: Run the benchmark

If all goes well, I'll have an executable component to run.

```
cabal run my-math-utilities:exe:bench
```
```
stack run my-math-utilities:exe:bench
```

Or, if I only compiled with `-rtsopts`, but not `-with-rtsopts=-T`,

```
cabal run my-math-utilities:exe:bench -- +RTS -T
```
```
stack run my-math-utilities:exe:bench -- +RTS -T
```

## Step 5: Clean up

After benchmarking, I can choose to keep the benchmark (and associated executable component) around, but I'll probably
delete them instead. I've learned something, collected some sweet screenshots for my PR, and I'm ready to move on.

# Caveat emptor

The statistical analysis performed by `park-bench` is simplistic, written by a novice, and may have bugs. Results should
not necessarily be trusted; please use (or at least compare to) a different tool.
