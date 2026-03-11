{-|
Module          : Text.Format
Copyright       : (c) 2019 Version Cloud
License         : BSD3
Maintainer      : Jorah Gao <jorah@version.cloud>
Stability       : experimental
Portability     : portable

This library is inspired by
Python's [str.format](https://docs.python.org/library/string.html) and
Haskell's [Text.Printf](http://hackage.haskell.org/package/base/docs/Text-Printf.html),
and most of the features are copied from these two libraries.
-}
module Text.Format
  ( -- * Format functions
    format
  , format1
    -- * Classes
  , FormatArg(formatArg)
  , FromArgKey(..)
  , FormatType(..)
  -- * Generic format arg and options
  , Options
  , defaultOptions
  , fieldLabelModifier
  , genericFormatArg
    -- * Data types
  , Formatter
  , Format
  , Format1
  , ArgKey(..)
  , topKey
  , popKey
  , ArgFmt(..)
  , prettyArgFmt
  , FmtAlign(..)
  , FmtSign(..)
  , FmtNumSep(..)
  , (:=) (..)
    -- * Standard Formatters
  , formatString
  , formatChar
  , formatInt
  , formatWord
  , formatInteger
  , formatRealFloat
  , defaultSpecs
    -- * Errors
  , ArgError(..)
  , errorArgKey
  , errorArgFmt
  , vferror
  ) where

import           Data.Map           (empty)

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Class
import           Text.Format.Error
import           Text.Format.Format


{-| Format a variable number of arguments with Python-style format string

>>> format "{:s}, {:d}, {:.4f}" "hello" 123 pi
"hello, 123, 3.1416"
>>> format "{1:s}, {0:d}, {2:.4f}" 123 "hello" pi
"hello, 123, 3.1416"
>>> format "{:s} {:d} {pi:.4f}" "hello" 123 ("pi" := pi)
"hello, 123, 3.1416"

See 'Format' to learn more about format string syntax.

See 'FormatArg' to learn how to derive FormatArg for your own data types.
-}
format :: FormatType r => Format -> r
format = flip sfmt empty

{-| A variant of 'format', it takes only one positional argument

>>> :set -XDeriveGeneric
>>> import GHC.Generics
>>> data Triple = Triple String Int Double deriving Generic
>>> instance FormatArg Triple
>>> format "{0!0:s} {0!1:d} {0!2:.4f}" $ Triple "hello" 123 pi
"hello, 123, 3.1416"
>>> format1 "{0:s} {1:d} {2:.4f}" $ Triple "hello" 123 pi
"hello, 123, 3.1416"
-}
format1 :: FormatArg a => Format1 -> a -> String
format1 = format . Format . unFormat1
