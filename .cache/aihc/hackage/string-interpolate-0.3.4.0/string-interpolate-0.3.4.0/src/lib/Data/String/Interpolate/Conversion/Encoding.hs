{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Strict         #-}

module Data.String.Interpolate.Conversion.Encoding
  ( bsToTextBuilder, lbsToTextBuilder, encodeCharUTF8 )
where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text.Lazy.Builder  as LT

import qualified "utf8-string" Data.ByteString.Lazy.UTF8 as LUTF8
import qualified "utf8-string" Data.ByteString.UTF8      as UTF8

-- |
-- Convert a strict ByteString into a Text `LT.Builder', converting any invalid
-- characters into the Unicode replacement character � (U+FFFD).
bsToTextBuilder :: B.ByteString -> LT.Builder
bsToTextBuilder = UTF8.foldr (\char bldr -> LT.singleton char <> bldr) mempty

-- |
-- Convert a lazy ByteString into a Text `LT.Builder', converting any invalid
-- characters into the Unicode replacement character � (U+FFFD).
lbsToTextBuilder :: LB.ByteString -> LT.Builder
lbsToTextBuilder = LUTF8.foldr (\char bldr -> LT.singleton char <> bldr) mempty

-- |
-- "Data.ByteString.Builder" provides `charUtf8' to do this, but it doesn't
-- correctly handle invalid characters.
encodeCharUTF8 :: Char -> LB.Builder
encodeCharUTF8 c =
  let normalized = case c of
        '\xFFFE' -> '\xFFFD'
        '\xFFFF' -> '\xFFFD'
        _        -> c
  in LB.charUtf8 normalized
