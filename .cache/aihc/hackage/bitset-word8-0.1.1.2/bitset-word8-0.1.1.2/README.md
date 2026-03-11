# bitset-word8

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
[![Build Status](https://github.com/nshimaza/bitset-word8/workflows/build/badge.svg)](https://github.com/nshimaza/bitset-word8/actions?query=workflow%3Abuild)
[![Hackage](https://img.shields.io/hackage/v/bitset-word8.svg?style=flat)](https://hackage.haskell.org/package/bitset-word8)
[![Stackage Nightly](http://stackage.org/package/bitset-word8/badge/nightly)](http://stackage.org/nightly/package/bitset-word8)
[![Stackage LTS](http://stackage.org/package/bitset-word8/badge/lts)](http://stackage.org/lts/package/bitset-word8)

Space efficient set of `Word8` and some pre-canned sets useful for parsing HTTP related `ByteString`.
This package is intended to provide O(1) membership test on any subset of ASCII and Latin-1 character set
in order to write efficient HTTP related parser.

### Creating your own set

You can create your own set by `fromList`.

```haskell
myCharSet :: BitSetWord8
myCharSet = fromList [ 'Y', 'y', 'N', 'n' ]
```

You can create pre-evaluated set using Template Haskell.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (Lift, lift)

myPreEvaluatedCharSet :: BitSetWord8
myPreEvaluatedCharSet = $(lift myCharSet)
```

### Example Usage

```haskell
import Data.Attoparsec.ByteString

-- | Parse RFC7230 token.
token :: Parser ByteString
token = takeWhile1 (member rfc7230TChar)
```
