# aeson-jsonpath

[![Build](https://github.com/taimoorzaeem/aeson-jsonpath/actions/workflows/build.yml/badge.svg)](https://github.com/taimoorzaeem/aeson-jsonpath/actions/workflows/build.yml) [![hackage-docs](https://img.shields.io/badge/hackage-v0.3.0.1-blue)](https://hackage.haskell.org/package/aeson-jsonpath) [![Donate](https://img.shields.io/badge/Donate-Patreon-red)](https://www.patreon.com/taimoorzaeem) [![Compliance](https://github.com/taimoorzaeem/aeson-jsonpath/actions/workflows/compliance.yml/badge.svg)](https://github.com/taimoorzaeem/aeson-jsonpath/actions/workflows/compliance.yml)

Run [RFC 9535](https://www.rfc-editor.org/rfc/rfc9535) compliant JSONPath queries on [Data.Aeson](https://hackage.haskell.org/package/aeson).

## Roadmap

- [x] Selectors
  - [x] Name Selector
  - [x] Index Selector
  - [x] Slice Selector
  - [x] Wildcard Selector
  - [x] Filter Selector
- [x] Segments
  - [x] Child Segment
  - [x] Descendant Segment
- [x] Normalized Paths
- [ ] Function Extensions

I have decided not to implement Function Extension yet. Please open an issue or discussion if you'd like to see them implemented.

## Quick Start

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.Aeson           (Value (..))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.JSONPath  (query, queryLocated, jsonPath)

track = [aesonQQ| { "artist": "Duster", "title": "Earth Moon Transit" } |]

ghci> query "$.artist" track -- child member shorthand
Right [String "Duster"]

ghci> queryLocated "$.*" track -- child wildcard segment
Right [
  ("$['artist']", String "Duster"),
  ("$['title']", String "Earth Moon Transit")
]
```

## More Examples

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.Aeson           (Value (..))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.JSONPath  (query, queryLocated, jsonPath)

json = [aesonQQ| {
  "shop": {
    "movies": [
      {
        "title": "Mandy",
        "director": "Panos Cosmatos",
        "year": 2018
      },
      {
        "title": "Laurence Anyways",
        "director": "Xavier Dolan",
        "year": 2012
      }
    ]
  }
}|]
```

### Child Segment

```haskell
ghci> query "$.shop.movies[0].title" json
Right [String "Mandy"]

ghci> query "$.shop.movies[0].*" json
Right [
  String "Mandy",
  String "Panos Cosmatos",
  Number 2018.0
]

ghci> query "$['shop']['new-movies']" json
Right []
```

### Descendant Segment

```haskell
-- get all values with key "director", recursively
ghci> query "$..director" json
Right [
  String "Panos Cosmatos",
  String "Xavier Dolan"
]
```

### Slice Selector

```haskell
ghci> query "$[2:5]" [aesonQQ| [1,2,3,4,5,6] |]
Right [
  Number 3.0,
  Number 4.0,
  Number 5.0
]
```

### Filter Selector

```haskell
ghci> query "$.shop.movies[?@.year < 2015]" json
Right [
  Object (fromList [
    ("director",String "Xavier Dolan"),
    ("title",String "Laurence Anyways"),
    ("year",Number 2012.0)
  ])
]

ghci> queryLocated "$.shop.movies[?@.director == 'Panos Cosmatos']" json
Right [
  (
    "$['shop']['movies'][0]",
    Object (fromList [
      ("director",String "Panos Cosmatos"),
      ("title",String "Mandy"),
      ("year",Number 2018.0)
    ])
  )
]
```

### QuasiQuoter

The functions `queryQQ` and `queryLocatedQQ` can be used with the `jsonPath` quasi quoter.

```haskell
queryQQ [jsonPath|$.shop.movies|] json -- compiles successfully

queryQQ [jsonPath|$.shop$$movies|] json -- compilation error, doesn't parse
```

## Testing

It is tested using 10000+ lines test suite given by [jsonpath-compliance-test-suite](https://github.com/jsonpath-standard/jsonpath-compliance-test-suite) :rocket:.

**Note:** All tests pass except tests related to **function extensions** which we have not implemented yet.

## Development

Please report any bugs you encounter by opening an issue.
