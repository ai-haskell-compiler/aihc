# optparse-text
[![Build Status](https://travis-ci.org/passy/optparse-text.svg?branch=master)](https://travis-ci.org/passy/optparse-text)

> Helpers for
> [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative)
> to deal with [`Data.Text`](https://hackage.haskell.org/package/text).

## Example

```haskell
import qualified Data.Text                 as T
import qualified Options.Applicative       as Opt
import qualified Options.Applicative.Text  as OptT

data Options = Options { text    :: T.Text
                       , textArg :: T.Text
                       , textOpt :: T.Text }
  deriving (Eq, Show)

optParser :: Opt.Parser Options
optParser = Options <$> Opt.argument OptT.text ( Opt.metavar "TEXT" )
                 <*> OptT.textArgument ( Opt.metavar "TEXT2" )
                 <*> OptT.textOption ( Opt.long "textopt" )
```

## Status

About to be published.

## Alternatives

You may want to consider using
[optparse-generic](https://github.com/Gabriel439/Haskell-Optparse-Generic-Library)
which has built-in support for Text and provides a very convient generic
interface to optparse avoiding a lot of boilerplate.
