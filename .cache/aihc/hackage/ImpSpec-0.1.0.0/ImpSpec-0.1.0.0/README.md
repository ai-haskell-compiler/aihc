# ImpSpec

__Imperative specification__.

Let a little imp help you discover all the bugs in your stateful Haskell program.

## Overview

`ImpSpec` is a testing library that is build on top of
[`hspec`](https://hackage.haskell.org/package/hspec) and
[`QuickCheck`](https://hackage.haskell.org/package/QuickCheck). It was designed for easier
testing of stateful applications.

## Status

| Github Actions | Coveralls | Hackage | Nightly | LTS |
|:--------------:|:---------:|:-------:|:-------:|:---:|
| [![Build Status][GA-badge]][GA-link] | [![Coverage Status][Coveralls-badge]][Coveralls-link] | [![Hackage][Hackage-badge]][Hackage-link] | [![Nightly][Nightly-badge]][Nightly-link] | [![LTS][LTS-badge]][LTS-link]

[GA-badge]: https://github.com/lehins/ImpSpec/workflows/CI/badge.svg
[GA-link]: https://github.com/lehins/ImpSpec/actions
[Coveralls-badge]: https://coveralls.io/repos/github/lehins/ImpSpec/badge.svg?branch=master
[Coveralls-link]: https://coveralls.io/github/lehins/ImpSpec?branch=master
[Hackage-badge]: https://img.shields.io/hackage/v/ImpSpec.svg
[Hackage-link]: https://hackage.haskell.org/package/ImpSpec
[Nightly-badge]: https://www.stackage.org/package/ImpSpec/badge/nightly
[Nightly-link]: https://www.stackage.org/nightly/package/ImpSpec
[LTS-badge]: https://www.stackage.org/package/ImpSpec/badge/lts
[LTS-link]: https://www.stackage.org/lts/package/ImpSpec
