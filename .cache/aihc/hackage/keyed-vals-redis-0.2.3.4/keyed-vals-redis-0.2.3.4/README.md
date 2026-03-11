# keyed-vals-redis

[![GitHub CI](https://github.com/adetokunbo/keyed-vals/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/keyed-vals/actions)
[![Stackage Nightly](http://stackage.org/package/keyed-vals-redis/badge/nightly)](http://stackage.org/nightly/package/keyed-vals-redis)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/keyed-vals-redis/blob/master/LICENSE)

`keyed-vals` aims to provide a narrow client for storing key-value collections
in storage services like [Redis] via an abstract [Handle] interface.

This package, `keyed-vals-redis`, provides an implementation of the [Handle]
that uses [Redis] as the storage service.


[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/keyed-vals-redis.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=keyed-vals-redis>
[hackage-badge]:      <https://img.shields.io/hackage/v/keyed-vals-redis.svg>
[hackage]:            <https://hackage.haskell.org/package/keyed-vals-redis>
[Handle]:             <https://hackage.haskell.org/package/keyed-vals-0.1.0.0/docs/KeyedVals-Handle.html>
[Redis]:              <https://redis.io>
