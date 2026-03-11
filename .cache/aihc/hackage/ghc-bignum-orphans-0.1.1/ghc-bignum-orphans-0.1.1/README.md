# `ghc-bignum-orphans`
[![Hackage](https://img.shields.io/hackage/v/ghc-bignum-orphans.svg)][Hackage: ghc-bignum-orphans]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/ghc-bignum-orphans.svg)](http://packdeps.haskellers.com/reverse/ghc-bignum-orphans)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/haskell-compat/ghc-bignum-orphans/workflows/Haskell-CI/badge.svg)](https://github.com/haskell-compat/ghc-bignum-orphans/actions?query=workflow%3AHaskell-CI)

[Hackage: ghc-bignum-orphans]:
  http://hackage.haskell.org/package/ghc-bignum-orphans
  "ghc-bignum-orphans package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`ghc-bignum-orphans` defines orphan instances that mimic instances available in
later versions of `ghc-bignum` to a wider (older) range of compilers.
`ghc-bignum-orphans` does not export anything except the orphan instances themselves.
