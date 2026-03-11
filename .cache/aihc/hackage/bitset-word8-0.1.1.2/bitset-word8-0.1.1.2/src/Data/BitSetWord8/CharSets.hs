{-|
Module      : Network.BitSetWord8.CharSets
Copyright   : (c) Naoto Shimazaki 2017
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

Some pre-canned character sets useful for HTTP related parsing.
All sets provided by this module are evaluated at compile time using Template Haskell.
-}

{-# LANGUAGE TemplateHaskell #-}

module Data.BitSetWord8.CharSets
    (
      rfc3986UriReference
    , rfc7230TChar
    , rfc7230QDText
    , rfc7230QuotedPair
    ) where

import           Language.Haskell.TH.Syntax (lift)

import           Data.BitSetWord8.Internal

-- | URI-Reference of RFC3986 in BitSetWord8.  Evaluated at compile time.
rfc3986UriReference :: BitSetWord8
rfc3986UriReference = $(lift $ fromList rfc3986UriReference')

-- | tcher of RFC7230 in BitSetWord8.  Evaluated at compile time.
rfc7230TChar :: BitSetWord8
rfc7230TChar = $(lift $ fromList rfc7230TChar')

-- | qdtext of RFC7230 in BitSetWord8.  Evaluated at compile time.
rfc7230QDText :: BitSetWord8
rfc7230QDText = $(lift $ fromList rfc7230QDText')

-- | quoted-string of RFC7230 in BitSetWord8.  Evaluated at compile time.
rfc7230QuotedPair :: BitSetWord8
rfc7230QuotedPair = $(lift $ fromList rfc7230QuotedPair')
