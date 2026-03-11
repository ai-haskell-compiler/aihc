# network-uri-template

[![Hackage](https://img.shields.io/hackage/v/network-uri-template.svg?style=flat)](https://hackage.haskell.org/package/network-uri-template)
[![Stackage Nightly](http://stackage.org/package/network-uri-template/badge/nightly)](http://stackage.org/nightly/package/network-uri-template)
[![Stackage LTS](http://stackage.org/package/network-uri-template/badge/lts)](http://stackage.org/lts/package/network-uri-template)

Library for parsing and expanding URI Templates, as per [RFC 6570][rfc6570].

[rfc6570]: https://datatracker.ietf.org/doc/html/rfc6570

## URI Templates

An example from the RFC:

> For example, the following URI Template includes a form-style
> parameter expression, as indicated by the "?" operator appearing
> before the variable names.
>
> ```
>   http://www.example.com/foo{?query,number}
> ```
>
> The expansion process for expressions beginning with the question-
> mark ("?") operator follows the same pattern as form-style interfaces
> on the World Wide Web:
>
> ```
>   http://www.example.com/foo{?query,number}
>                             \_____________/
>                                |
>                                |
>           For each defined variable in [ 'query', 'number' ],
>           substitute "?" if it is the first substitution or "&"
>           thereafter, followed by the variable name, '=', and the
>           variable's value.
> ```
>
> If the variables have the values
>
> ```
>   query  := "mycelium"
>   number := 100
> ```
>
> then the expansion of the above URI Template is
>
> ```
>   http://www.example.com/foo?query=mycelium&number=100
> ```
>
> Alternatively, if 'query' is undefined, then the expansion would be
>
> ```
>   http://www.example.com/foo?number=100
> ```
>
> or if both variables are undefined, then it would be
>
> ```
>   http://www.example.com/foo
> ```

For a complete description of URI Templates, consult the RFC or see our
extracted [test cases][examples].

[examples]: rfc/examples.txt

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Prelude

import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Network.URI.Template
import System.Exit (die)

main :: IO ()
main = do
  let
    vars :: Map VarName VarValue
    vars =
      Map.fromList
        [ ("query", "mycelium")
        , ("number", "100")
        ]

    template :: Text
    template = "http://www.example.com/foo{?query,number}"

    handle = either (die . templateErrorPretty) T.putStrLn

  handle $ processTemplate vars template
  -- => http://www.example.com/foo?query=mycelium&number=100

  handle $ processTemplate (Map.delete "query" vars) template
  -- => http://www.example.com/foo?number=100

  handle $ processTemplate (Map.empty) template
  -- => http://www.example.com/foo
```

## CLI

This project includes a small CLI to experiment with template expansion.

```console
network-uri-template \
  --var 'query  := "mycelium"' \
  --var 'number := "100"' \
  --var 'path   := ("foo", "bar")' \
  --var 'keys   := [("sort","asc"), ("page","2")]' \
  'http://www.example.com{/path*}/foo{?query:2,number}{&keys*}'
```

![](./cli.png)

## Similar Libraries

1. **[uri-template](https://hackage.haskell.org/package/uri-template)**: very
   basic library, not updated since 2008. Unlikely to be well-tested or support
   the full RFC.

1. **[uri-templater](https://hackage.haskell.org/package/uri-templater)**: looks
   more feature-complete, examples cover the full RFC. Built as a quasi-quoter
   that interpolates Haskell variables from the surrounding scope. Not clear how
   to expand with run-time values.

## License

This project is licensed AGPLv3. See [COPYING](./COPYING).
