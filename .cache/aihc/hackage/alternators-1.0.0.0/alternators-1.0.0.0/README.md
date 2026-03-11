[![Hackage](https://img.shields.io/hackage/v/alternators.svg)](https://hackage.haskell.org/package/alternators)
[![Build Status](https://secure.travis-ci.org/louispan/alternators.png?branch=master)](http://travis-ci.org/louispan/alternators)

`MonadDelegate` assists with coding the handlers for continuation-like monads.

# Changelog

* 1.0.0.0
  - Removed `runReaderM` as it can be replaced with `lift`, `hoist` and `>>=`
    See [gist](https://gist.github.com/louispan/1c7792d45ebe5559ffc45aa9db461c35)
  - Replace `fromMaybeT` with `evalMaybeT`.
  - Added `MonadDelegate` for a more accessing a continuation in the same monad.
