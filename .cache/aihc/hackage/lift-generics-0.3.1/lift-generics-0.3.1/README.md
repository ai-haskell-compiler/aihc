# `lift-generics`

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/lift-generics.svg)](http://packdeps.haskellers.com/reverse/lift-generics)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/RyanGlScott/lift-generics/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/lift-generics/actions?query=workflow%3AHaskell-CI)

[Hackage: lift-generics]:
  http://hackage.haskell.org/package/lift-generics
    "lift-generics package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package provides a `GHC.Generics`-based `genericLift` function, which can
be used for providing a `Language.Haskell.TH.Syntax.lift` implementation. See
the documentation in the `Language.Haskell.TH.Lift.Generics` module to get
started.

Credit goes to Matthew Pickering for [suggesting this
idea](https://ghc.haskell.org/trac/ghc/ticket/1830#comment:12).
