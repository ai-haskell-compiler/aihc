# `foldable1-classes-compat`
[![Hackage](https://img.shields.io/hackage/v/foldable1-classes-compat.svg)][Hackage: foldable1-classes-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/foldable1-classes-compat.svg)](http://packdeps.haskellers.com/reverse/foldable1-classes-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://github.com/haskell-compat/foldable1-classes-compat/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-compat/foldable1-classes-compat/actions?query=workflow%3AHaskell-CI)

[Hackage: foldable1-classes-compat]:
  http://hackage.haskell.org/package/foldable1-classes-compat
  "foldable1-classes-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

A compatibility package for the `Foldable1` and `Bifoldable1` type classes,
which were introduced in `base-4.18.0.0` (GHC 9.6.1). For more information, see
[this Core Libraries Committee
proposal](https://github.com/haskell/core-libraries-committee/issues/9).

`Foldable1` and `Bifoldable1` classify non-empty data structures that can be
folded to a summary value.
