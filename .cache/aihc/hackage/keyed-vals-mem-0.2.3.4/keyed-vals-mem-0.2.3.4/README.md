# keyed-vals-mem

[![GitHub CI](https://github.com/adetokunbo/keyed-vals/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/keyed-vals/actions)
[![Stackage Nightly](http://stackage.org/package/keyed-vals-mem/badge/nightly)](http://stackage.org/nightly/package/keyed-vals-mem)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/keyed-vals-mem/blob/master/LICENSE)

`keyed-vals` aims to provide a narrow client for storing key-value collections
in storage services like [Redis] via an abstract [Handle] interface.

This package, `keyed-vals-mem`, provides an in-memory implementation of the [Handle]
suitable for use in testing and prototyping.

[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/keyed-vals-mem.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=keyed-vals-mem>
[hackage-badge]:      <https://img.shields.io/hackage/v/keyed-vals-mem.svg>
[hackage]:            <https://hackage.haskell.org/package/keyed-vals-mem>
[Handle]:             <https://hackage.haskell.org/package/keyed-vals-0.1.0.0/docs/KeyedVals-Handle.html>
[Redis]:              <https://redis.io>
