# Koji XML-RPC binding for Haskell

[![GitHub CI](https://github.com/juhp/koji-hs/workflows/build/badge.svg)](https://github.com/juhp/koji-hs/actions)
[![Hackage](https://img.shields.io/hackage/v/koji.svg?logo=haskell)](https://hackage.haskell.org/package/koji)
[![Stackage Lts](http://stackage.org/package/koji/badge/lts)](http://stackage.org/lts/package/koji)
[![Stackage Nightly](http://stackage.org/package/koji/badge/nightly)](http://stackage.org/nightly/package/koji)
[![GPL-2 license](https://img.shields.io/badge/license-GPL--2-blue.svg)](LICENSE)

[Koji](https://pagure.io/koji/) is a distributed RPM-based buildsystem.

This koji-hs project provides the Haskell koji package,
a library with bindings to the Koji XML-RPC API.

It is a WIP: currently only querying koji is working,
ie login authentication or building etc is supported yet.

## Building

Build with `stack` or `cabal new-build`.

## Usage

Due to haxr using HsOpenSSL, programs using this library
need to be linked with `ghc-options: -threaded`, otherwise
the ghc RTS will complain with a runtime error.

See the Haddock documentation for more details.

## Projects using koji-hs

- [fbrnch](https://github.com/juhp/fbrnch/)
- [koji-progress](https://github.com/juhp/koji-progress)
- [pkgtreediff](https://github.com/juhp/pkgtreediff/)
