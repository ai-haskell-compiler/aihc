# `thread-local-storage`
[![Hackage](https://img.shields.io/hackage/v/thread-local-storage.svg)][Hackage: thread-local-storage]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/thread-local-storage.svg)](http://packdeps.haskellers.com/reverse/thread-local-storage)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/rrnewton/thread-local-storage.svg)](https://travis-ci.org/rrnewton/thread-local-storage)

[Hackage: thread-local-storage]:
  http://hackage.haskell.org/package/thread-local-storage
  "thread-local-storage package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

See .cabal file for description.

Here are some example benchmark results (on an Ivy Bridge i7-3770), with a typical time of 10.4 nanoseconds to get to a TLS variable given one IO thread per OS thread:

![example benchmarks](https://raw.githubusercontent.com/rrnewton/thread-local-storage/master/bench/example_results.png)
