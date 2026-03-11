-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.ByteString.Conversion.To
    ( ToByteString (..)
    , toByteString
    , toByteString'
    , runBuilder
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Conversion.Internal
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra hiding (runBuilder)
import Data.CaseInsensitive (CI, original)
import Data.Int
import Data.List (intersperse)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import GHC.Float (float2Double)

import qualified Data.ByteString.Lazy    as L
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

#ifdef WINDOWS
import Blaze.Text.Double
#else
import Data.Double.Conversion.Text
#endif

class ToByteString a where
    builder :: a -> Builder

instance ToByteString Builder      where builder x = x
instance ToByteString L.ByteString where builder x = lazyByteString x
instance ToByteString ByteString   where builder x = byteString x
instance ToByteString Text         where builder x = byteString $ encodeUtf8 x
instance ToByteString TL.Text      where builder x = lazyByteString $ TL.encodeUtf8 x
instance ToByteString Char         where builder x = builder $ T.singleton x
instance ToByteString [Char]       where builder x = builder $ TL.pack x
#ifdef WINDOWS
instance ToByteString Float        where builder x = double $ float2Double x
instance ToByteString Double       where builder x = double x
#else
instance ToByteString Float        where builder x = builder $ toShortest $ float2Double x
instance ToByteString Double       where builder x = builder $ toShortest x
#endif

instance ToByteString Int          where builder x = intDec x
instance ToByteString Int8         where builder x = int8Dec x
instance ToByteString Int16        where builder x = int16Dec x
instance ToByteString Int32        where builder x = int32Dec x
instance ToByteString Int64        where builder x = int64Dec x
instance ToByteString Integer      where builder x = integerDec x
instance ToByteString Word         where builder x = wordDec x
instance ToByteString Word8        where builder x = word8Dec x
instance ToByteString Word16       where builder x = word16Dec x
instance ToByteString Word32       where builder x = word32Dec x
instance ToByteString Word64       where builder x = word64Dec x

instance ToByteString (Hex Int)    where builder (Hex x) = sign x <> wordHex (toWord x)
instance ToByteString (Hex Int8)   where builder (Hex x) = sign x <> word8Hex (toWord x)
instance ToByteString (Hex Int16)  where builder (Hex x) = sign x <> word16Hex (toWord x)
instance ToByteString (Hex Int32)  where builder (Hex x) = sign x <> word32Hex (toWord x)
instance ToByteString (Hex Int64)  where builder (Hex x) = sign x <> word64Hex (toWord x)
instance ToByteString (Hex Word)   where builder (Hex x) = wordHex x
instance ToByteString (Hex Word8)  where builder (Hex x) = word8Hex x
instance ToByteString (Hex Word16) where builder (Hex x) = word16Hex x
instance ToByteString (Hex Word32) where builder (Hex x) = word32Hex x
instance ToByteString (Hex Word64) where builder (Hex x) = word64Hex x

instance ToByteString a => ToByteString (List a) where
    builder = mconcat . intersperse comma . map builder . fromList

instance ToByteString Bool where
    builder True  = byteString "true"
    builder False = byteString "false"

instance ToByteString a => ToByteString (CI a) where
    builder = builder . original

toByteString :: ToByteString a => a -> L.ByteString
toByteString = runBuilder . builder

-- | Please note that this needs to convert from a lazy 'L.ByteString' to
-- a strict one which involves copying the whole string.
toByteString' :: ToByteString a => a -> ByteString
toByteString' = L.toStrict . toByteString

runBuilder :: Builder -> L.ByteString
runBuilder = toLazyByteStringWith (safeStrategy 32 smallChunkSize) L.empty

sign :: Integral a => a -> Builder
sign n = if n < 0 then minus else mempty
{-# SPECIALISE sign :: Int   -> Builder #-}
{-# SPECIALISE sign :: Int8  -> Builder #-}
{-# SPECIALISE sign :: Int16 -> Builder #-}
{-# SPECIALISE sign :: Int32 -> Builder #-}
{-# SPECIALISE sign :: Int64 -> Builder #-}

toWord :: (Integral a, Integral b) => a -> b
toWord = fromIntegral . abs
{-# SPECIALISE toWord :: Int   -> Word   #-}
{-# SPECIALISE toWord :: Int8  -> Word8  #-}
{-# SPECIALISE toWord :: Int16 -> Word16 #-}
{-# SPECIALISE toWord :: Int32 -> Word32 #-}
{-# SPECIALISE toWord :: Int64 -> Word64 #-}

comma, minus :: Builder
comma = byteString ","
minus = byteString "-"
{-# INLINE comma #-}
{-# INLINE minus #-}
