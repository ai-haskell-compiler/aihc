[![Hackage](https://img.shields.io/hackage/v/http-link-header?style=flat) ![](https://img.shields.io/endpoint?url=https://hackage-downloads-badge.deno.dev/http-link-header)](https://hackage.haskell.org/package/http-link-header)
[![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](https://unlicense.org)
[![Support me on Patreon](https://img.shields.io/badge/dynamic/json?logo=patreon&color=%23e85b46&label=support%20me%20on%20patreon&query=data.attributes.patron_count&suffix=%20patrons&url=https%3A%2F%2Fwww.patreon.com%2Fapi%2Fcampaigns%2F9395291)](https://www.patreon.com/valpackett)

# http-link-header

A Haskell library than implements a parser and a writer for the HTTP Link header as specified in [RFC 5988 "Web Linking"](https://tools.ietf.org/html/rfc5988).

## Usage

```haskell
import Network.HTTP.Link
import Network.URI
import Data.Maybe

----- Writing
writeLinkHeader [ Link (fromJust $ parseURI "https://example.com/hello%20world") [(Rel, "next"), (Title, "hello world")]
                , Link (fromJust $ parseURI "https://yolo.tld") [(Rel, "license")] ]
-- "<https://example.com/hello%20world>; rel=\"next\"; title=\"hello world\", <https://yolo.tld>; rel=\"license\""

----- Parsing
parseLinkHeader "<https://example.com/2>; rel=\"next\", <https://example.com/0>; rel=prev"
-- Just [ Link https://example.com/2 [(Rel, "next")]
--      , Link https://example.com/0 [(Rel, "prev")] ]
```

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

```bash
$ stack build

$ stack test && rm tests.tix

$ stack bench

$ stack ghci --ghc-options="-fno-hpc"
```

[stack]: https://github.com/commercialhaskell/stack

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](https://unlicense.org).
