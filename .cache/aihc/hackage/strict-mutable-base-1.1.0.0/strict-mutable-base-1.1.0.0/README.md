# strict-mutable-base

[![Hackage version](https://img.shields.io/hackage/v/strict-mutable-base.svg?label=Hackage)](https://hackage.haskell.org/package/strict-mutable-base)
[![Build Status](https://github.com/arybczak/strict-mutable/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/arybczak/strict-mutable/actions?query=branch%3Amaster)
[![Dependencies](https://img.shields.io/hackage-deps/v/strict-mutable-base.svg)](https://packdeps.haskellers.com/feed?needle=andrzej@rybczak.net)
[![Stackage LTS](https://www.stackage.org/package/strict-mutable-base/badge/lts)](https://www.stackage.org/lts/package/strict-mutable-base)
[![Stackage Nightly](https://www.stackage.org/package/strict-mutable-base/badge/nightly)](https://www.stackage.org/nightly/package/strict-mutable-base)

Strict (WHNF) variants of
[Chan](https://hackage.haskell.org/package/base/docs/Control-Concurrent-Chan.html),
[IORef](https://hackage.haskell.org/package/base/docs/Data-IORef.html) and
[MVar](https://hackage.haskell.org/package/base/docs/Control-Concurrent-MVar.html)
for proactive prevention of space leaks.
