data-or
=======
[![CI Status](https://github.com/wrengr/data-or/actions/workflows/ci.yml/badge.svg)](https://github.com/wrengr/data-or/actions?query=workflow%3Aci+-event%3Apull_request)
[![Hackage version](https://img.shields.io/hackage/v/data-or.svg?style=flat)](https://hackage.haskell.org/package/data-or) 
[![Stackage LTS version](https://stackage.org/package/data-or/badge/lts)](https://stackage.org/lts/package/data-or)
[![Stackage Nightly version](https://stackage.org/package/data-or/badge/nightly)](https://stackage.org/nightly/package/data-or)

This package provides a data type for non-exclusive disjunction.
In addition we provide a non-truncating version of `zip`, `zipWith`,
etc.


## Install

This is a simple package and should be easy to install. You should
be able to use the standard:

    $> cabal install data-or
    

## Portability

An attempt has been made to keep this library as portable as possible.
It is Haskell98 except for the use of CPP, which allows some functions
to be good producers for list fusion in GHC.


## Links

* [Website](http://wrengr.org/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/data-or)
* [GitHub](https://github.com/wrengr/data-or)
