# redis-glob

[![GitHub CI](https://github.com/adetokunbo/redis-glob/actions/workflows/ci.yml/badge.svg)](https://github.com/adetokunbo/redis-glob/actions)
[![Stackage Nightly](http://stackage.org/package/redis-glob/badge/nightly)](http://stackage.org/nightly/package/redis-glob)
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD3](https://img.shields.io/badge/license-BSD3-green.svg?dummy)](https://github.com/adetokunbo/redis-glob/blob/master/LICENSE)

`redis-glob` checks that glob expressions for use with Redis are valid.
Redis commands like [KEYS] use glob expressions to match keys in redis.

If the glob expression is invalid, the command returns an empty result, which
unfortunately is the same as if the expression was valid but had no results.

Use `validate` to confirm if a glob expression is valid. It parses the
expression in the same way as the actual [redis glob] implementation, returning
`Nothing` for invalid expressions.

`matches` can be used to confirm that a bytestring matches a glob expression if
the the glob expression is valid.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Redis.Glob
import qualified Data.ByteString.Lazy.Char8 as CL


isOK :: CL.ByteString -> IO ()
isOk b = do
  CL.putStrLn $ "Is " <> b <> " ok? " <> maybe "n" (const "y") (validate b)
  CL.putStrLn $ "Does it match hello? " <> if (b `matches` "hello") then "y" else "n"

main :: IO()
main = do
  isOK "h?llo"     -- Is h?llo ok? y
                   -- Does it match hello? y
  isOK "y[a-b]llo" -- Is y[a-b]llo ok? y"
                   -- Does it match hello? n
  isOK "h[a-]llo"  -- Is h[a-]llo ok? n
                   -- Does it match hello? n
```

[1]: https://hackage.haskell.org/package/wai
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/redis-glob.svg>
[hackage-deps]:       <http://packdeps.haskellers.com/feed?needle=redis-glob>
[hackage-badge]:      <https://img.shields.io/hackage/v/redis-glob.svg>
[hackage]:            <https://hackage.haskell.org/package/redis-glob>
[KEYS]:               <https://redis.io/commands/keys>
[redis glob]:         <https://github.com/redis/redis/blob/203b12e41ff7981f0fae5b23819f072d61594813/src/util.c#L54>
