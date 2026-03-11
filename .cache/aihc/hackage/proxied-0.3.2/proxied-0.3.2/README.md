# `proxied`
[![Hackage](https://img.shields.io/hackage/v/proxied.svg)][Hackage: proxied]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/proxied.svg)](http://packdeps.haskellers.com/reverse/proxied)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/RyanGlScott/proxied/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/proxied/actions?query=workflow%3AHaskell-CI)

[Hackage: proxied]:
  http://hackage.haskell.org/package/proxied
  "proxied package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

`proxied` is a simple library that exports a function to convert constant functions to ones that take a `proxy` value in the `Data.Proxied`. This is useful for retrofiting typeclasses that have functions that return a constant value for any value of a particular type (but still need to consume some value, since one of the parameterized types must appear in a typeclass function). Often, these functions are given `undefined` as an argument, which might be considered poor design.

 `Proxy`, however, does not carry any of the error-throwing risks of `undefined`, so it is much more preferable to take `Proxy` as an argument to a constant function instead of `undefined`. Unfortunately, `Proxy` wasn't included in `base` until GHC 7.8, so many of `base`'s typeclasses still contain constant functions that aren't amenable to passing `Proxy`. `proxied` addresses this issue by providing variants of those typeclass functions that take an explicit `proxy` value.

This library also contains the `Data.Proxyless` module, which works in the opposite direction. That is, one can take functions which take `Proxy` (or `undefined`) as an argument and convert them to functions which take no arguments. This trick relies on the `-XTypeApplications` extension, so it is only available with GHC 8.0 or later. This library also offers `Data.Proxyless.RequiredTypeArguments`, a variant of `Data.Proxyless` that uses `-XRequiredTypeArguments` to make type arguments explicit, which is only available with GHC 9.10 or later.
