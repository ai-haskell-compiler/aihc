# pvar

Interface for a mutable variable `PVar` that can hold values that have `Prim` instance.

## Status

| Language | Travis | Azure | Coveralls |
|:--------:|:------:|:-----:|:---------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/pvar.svg) | [![Travis](https://img.shields.io/travis/lehins/pvar/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/lehins/pvar) | [![Build Status](https://dev.azure.com/kuleshevich/pvar/_apis/build/status/lehins.pvar?branchName=master)](https://dev.azure.com/kuleshevich/pvar/_build/latest?definitionId=1?branchName=master) | [![Coverage Status](https://coveralls.io/repos/github/lehins/pvar/badge.svg?branch=master)](https://coveralls.io/github/lehins/pvar?branch=master)

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`pvar`](https://github.com/lehins/pvar)| [![Hackage](https://img.shields.io/hackage/v/pvar.svg)](https://hackage.haskell.org/package/pvar)| [![Nightly](https://www.stackage.org/package/pvar/badge/nightly)](https://www.stackage.org/nightly/package/pvar)| [![Nightly](https://www.stackage.org/package/pvar/badge/lts)](https://www.stackage.org/lts/package/pvar)

# Overview

Main features include:

* Performance. There is practically no overhead when compared to operating on pure values,
  although there is a higher memory overhead, since `PVar` is backed by a
  `MutableByteArray#`
* Atomic operations for `PVar`s with `Int` values. This includes a unique function that is
  not available in `ghc-prim` out of the box:

```haskell
atomicModifyIntPVar :: PrimMonad m => PVar m Int -> (Int -> (Int, a)) -> m a
```

* Works in `PrimMonad`, therefore it is usable with `ST`, `IO` and various transformer monads.
* Easy access to `PVar` contents with `Storable`
* `isByteArrayPinned`, `isMutableByteArrayPinned` function that work on ghc-7.10 and
  ghc-8.0 as well as all the newer ones.
