# optparse-enum

An `enum-text` based toolkit for `optparse-applicative`.

A simple but complete example:

```haskell
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}

import Fmt
import Text.Enum.Optparse
import Paths_optparse_enum

data Choice
  = C_version
  | C_hello
  deriving (Bounded,Enum,EnumText,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText Choice

parserDetails ::   ParserDetails
parserDetails =
  ParserDetails
    { _pd_desc   = "optparse-enum example program"
    , _pd_header = "A simple optparse-enum illustrative program"
    , _pd_footer = "See the optparse-enum page on Hackage for details"
    }

main :: IO ()
main = do
  choice <- parseIO parserDetails enumSwitchesP
  case choice of
    C_version -> print    version
    C_hello   -> putStrLn "Hello!"
```
