[![Hackage](https://img.shields.io/hackage/v/pcre-heavy.svg?style=flat) ![](https://img.shields.io/endpoint?url=https://hackage-downloads-badge.deno.dev/pcre-heavy)](https://hackage.haskell.org/package/pcre-heavy)
[![unlicense](https://img.shields.io/badge/un-license-green.svg?style=flat)](https://unlicense.org)
[![Support me on Patreon](https://img.shields.io/badge/dynamic/json?logo=patreon&color=%23e85b46&label=support%20me%20on%20patreon&query=data.attributes.patron_count&suffix=%20patrons&url=https%3A%2F%2Fwww.patreon.com%2Fapi%2Fcampaigns%2F9395291)](https://www.patreon.com/valpackett)

# pcre-heavy

A Haskell regular expressions library with support for multiple matches and replacements:

- based on [pcre-light], none of that regex-base complicated pluggable-backend stuff
- takes and returns [ConvertibleStrings] everywhere, use any common string type (String, ByteString, Lazy ByteString, Text, Lazy Text) -- but you need a bit more type annotations (or [ClassyPrelude]'s `asText`, `asString`, etc.) if you use `OverloadedStrings` which you probably can't live without
- provides a QuasiQuoter for regexps that does compile time checking

[pcre-light]: https://hackage.haskell.org/package/pcre-light
[ConvertibleStrings]: https://hackage.haskell.org/package/string-conversions
[ClassyPrelude]: https://hackage.haskell.org/package/classy-prelude

## Usage

```haskell
{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}
import           Text.Regex.PCRE.Heavy
```

### Checking

```haskell
>>> "https://val.packett.cool" =~ [re|^http.*|]
True
```

For `UnicodeSyntax` fans, it's also available as ≈ (U+2248 ALMOST EQUAL TO):

```haskell
>>> "https://val.packett.cool" ≈ [re|^http.*|]
True
```

### Matching (Searching)

(You can use any string type, not just String!)

`scan` returns all matches as pairs like `(fullmatch, [group, group...])`.

```haskell
>>> scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi" :: [(String, [String])]
[
  (" entry 1 hello  &", ["1", "hello"])
, ("entry 2 hi",        ["2", "hi"])
]
```

It is lazy!
If you only need the first match, use `head` (or, much better, `headMay` from [safe]) -- no extra work will be performed!

```haskell
>>> headMay $ scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi"
Just (" entry 1 hello  &", ["1", "hello"])
```

[safe]: https://hackage.haskell.org/package/safe

### Replacement

`sub` replaces the first match, `gsub` replaces all matches.

```haskell
-- You can use a convertible string type `a` as the replacement...
>>> gsub [re|\d+|] "!!!NUMBER!!!" "Copyright (c) 2015 The 000 Group"
"Copyright (c) !!!NUMBER!!! The !!!NUMBER!!! Group"

-- or a ([a] -> a) function -- that will get the groups...
>>> gsub [re|%(\d+)(\w+)|] (\(d:w:_) -> "{" ++ d ++ " of " ++ w ++ "}" :: String) "Hello, %20thing"
"Hello, {20 of thing}"

-- or a (a -> a) function -- that will get the full match...
>>> gsub [re|-\w+|] (\x -> "+" ++ (reverse $ drop 1 x) :: String) "hello -world"
"hello +dlrow"

-- or a (a -> [a] -> a) function.
-- That will get both the full match and the groups.
-- I have no idea why you would want to use that, but that's there :-)
```

Note that functions are the _only_ way to use captured groups in the replacement. There is no "in string" syntax like in Perl or in Python.

### Splitting

`split`, well, splits.

```haskell
>>> split [re|%(begin|next|end)%|] "%begin%hello%next%world%end%"
["","hello","world",""]
```

### Options

You can pass `pcre-light` options by using the `somethingO` variants of functions (and `mkRegexQQ` for compile time options):

```haskell
>>> let myRe = mkRegexQQ [multiline, utf8, ungreedy]
>>> scanO [myRe|\s*entry (\d+) (\w+)\s*&?|] [exec_no_utf8_check] " entry 1 hello  &entry 2 hi" :: [[String]]
>>> gsubO [myRe|\d+|] [exec_notempty] "!!!NUMBER!!!" "Copyright (c) 2015 The 000 Group"
```

`utf8` is passed by default in the `re` QuasiQuoter.

## Development

Use [stack] to build.  
Use ghci to run tests quickly with `:test` (see the `.ghci` file).

```bash
$ stack build

$ stack test && rm tests.tix

$ stack ghci --ghc-options="-fno-hpc"
```

[stack]: https://github.com/commercialhaskell/stack

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](https://unlicense.org).
