{-|
Module      : Network.BitSetWord8
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Space efficient set of 'Word8' and some pre-canned sets useful for parsing HTTP related 'ByteString'.
This packaged is intended to provide O(1) membership test on any subset of ASCII and Latin-1 character set
in order to write efficient HTTP related parser.

=== Creating your own set

You can create your own set by 'fromList'.

@
myCharSet :: BitSetWord8
myCharSet = fromList [ 'Y', 'y', 'N', 'n' ]
@

You can create pre-evaluated set using Template Haskell.

@
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax (Lift, lift)

myPreEvaluatedCharSet :: BitSetWord8
myPreEvaluatedCharSet = $(lift myCharSet)
@

=== Example Usage

@
import Data.Attoparsec.ByteString

-- | Parse RFC7230 token.
token :: Parser ByteString
token = takeWhile1 (member rfc7230TChar)
@
-}

module Data.BitSetWord8
    (
    -- * Types
      BitSetWord8
    -- * Useful charset constants in BitSetWord8
    , rfc3986UriReference
    , rfc7230TChar
    , rfc7230QDText
    , rfc7230QuotedPair
    -- * Source Constants
    , rfc3986UriReference'
    , rfc7230QDText'
    , rfc7230QuotedPair'
    , rfc5234Digit'
    , rfc2616UpAlpha'
    , rfc2616LoAlpha'
    , rfc5234Alpha'
    , rfc5234HexDig'
    , rfc5234VChar'
    , rfc5324Wsp'
    , rfc3986SubDelims'
    , rfc3986GenDelims'
    , rfc3986Reserved'
    , rfc3986Unreserved'
    , rfc3986PctEncodedChar'
    , rfc3986PChar'
    , rfc7230TChar'
    , rfc7230ObsText'
    -- * Functions
    , fromList
    , member
    ) where

import           Data.BitSetWord8.CharSets
import           Data.BitSetWord8.Internal
