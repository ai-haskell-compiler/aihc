prelude-safeenum
================
[![CI Status](https://github.com/wrengr/prelude-safeenum/actions/workflows/ci.yml/badge.svg)](https://github.com/wrengr/prelude-safeenum/actions?query=workflow%3Aci+-event%3Apull_request)
[![Hackage version](https://img.shields.io/hackage/v/prelude-safeenum.svg?style=flat)](https://hackage.haskell.org/package/prelude-safeenum)
[![Stackage LTS version](https://stackage.org/package/prelude-safeenum/badge/lts)](https://stackage.org/lts/package/prelude-safeenum)
[![Stackage Nightly version](https://stackage.org/package/prelude-safeenum/badge/nightly)](https://stackage.org/nightly/package/prelude-safeenum)

A redefinition of the Prelude's `Enum` class in order to render it
safe. That is, the Haskell Language Report defines `pred`, `succ`,
`fromEnum`, and `toEnum` to be partial functions when the type is
`Bounded`. This is unacceptable. We define a new type-class hierarchy
for enumeration which is safe and also generalizes to cover types
which can only be enumerated in one direction.


## Install

This is a very simple package and should be easy to install. You
should be able to use the standard:

    $> cabal install prelude-safeenum


## Portability

An attempt has been made to keep this library portable; however,
it does rely on a few language extensions. All the required language
extensions are:

* CPP
* GeneralizedNewtypeDeriving
* MagicHash - only for GHC
* Trustworthy - only for GHC >= 7.1

The GeneralizedNewtypeDeriving extension is used for brevity in
Data.Number.CalkinWilf. If you'd like to use this package with a
compiler that does not support that extension, contact the maintainer
and it can be removed.

This package is only "Trustworthy" rather than "Safe" for two
reasons: (1) Data.Number.CalkinWilf uses GeneralizedNewtypeDeriving,
and (2) Prelude.SafeEnum imports GHC.Exts for build/foldr fusion
and for the Char instances.


## Links

* [Website](http://wrengr.org)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/prelude-safeenum)
* [GitHub](https://github.com/wrengr/prelude-safeenum)
