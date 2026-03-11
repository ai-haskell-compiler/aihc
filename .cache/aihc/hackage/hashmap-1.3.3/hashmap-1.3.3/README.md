# `hashmap`
[![Hackage](https://img.shields.io/hackage/v/hashmap.svg)][Hackage: hashmap]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/hashmap.svg)](http://packdeps.haskellers.com/reverse/hashmap)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build](https://img.shields.io/travis/foxik/hashmap.svg)](https://travis-ci.org/foxik/hashmap)

[Hackage: hashmap]:
  http://hackage.haskell.org/package/hashmap
  "hashmap package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

:warning: Deprecated in favor of [`unordered-containers`](https://github.com/tibbe/unordered-containers)

An implementation of persistent `Map` and `Set` containers based on hashing. The implementation is build on top of `Data.IntMap.IntMap` and `Data.IntSet.IntSet`, with very similar API. It uses `Hashable` class from the `hashable` package for hashing.

This package can be used as a drop-in replacement for `Data.Map` and `Data.Set` modules.

The `Map key value` is an `Data.IntMap.IntMap` indexed by the hash value, containing either one (`key`, `value`) or a `Data.Map.Map key value` for all keys with the same hash value.

The `Set elem` is an `Data.IntMap.IntMap` indexed by the hash value, containing either one `elem` or `Data.Set.Set elem` for all elements with the same hash value.
