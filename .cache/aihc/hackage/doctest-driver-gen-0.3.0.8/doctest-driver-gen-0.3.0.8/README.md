# doctest-driver-gen

[![Hackage version](https://img.shields.io/hackage/v/doctest-driver-gen.svg?label=Hackage)](http://hackage.haskell.org/package/doctest-driver-gen)
[![Stackage LTS](https://www.stackage.org/package/doctest-driver-gen/badge/lts)](https://www.stackage.org/lts/package/doctest-driver-gen)
[![Stackage Nightly](https://www.stackage.org/package/doctest-driver-gen/badge/nightly)](https://www.stackage.org/nightly/package/doctest-driver-gen)

doctest-driver-gen is a doctest's driver file generator. It lets you automatically generate driver file for [doctest's cabal integration](https://github.com/sol/doctest/blob/f1f6c4149283036a55f9ce7f50439fabf0b9a0ab/README.md#cabal-integration).

Versions of doctest available in this library: `>= 0.7 && < 0.12 || >= 0.13 && < 0.23`

## Usage

```haskell
{-# OPTIONS_GHC -F -pgmF doctest-driver-gen [-optF DOCTEST_OPTION]... #-}
```

Make your driver file with the content. There is an example:

```haskell
{-# OPTIONS_GHC -F -pgmF doctest-driver-gen -optF src -optF app #-}
```

## Motivation

This package was motivated by [doctest-discover](https://hackage.haskell.org/package/doctest-discover). I want a simpler doctest's driver file generator.
