{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -O2 #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-unfoldings -dsuppress-idinfo -dsuppress-module-prefixes -ddump-to-file #-}

-- |
-- Module      : Data.Serialize.LEB128
-- Description : LEB128 encoding
-- License     : MIT
-- Maintainer  : Joachim Breitner
--
-- | This module implements encoding and decoding of 'Natural' and 'Integer'
-- values according to LEB128 and SLEB128. See
-- https://en.wikipedia.org/wiki/LEB128 for a specification.
--
-- The module provides conversion to and from strict bytestrings.
--
-- Additionally, to integrate these into your own parsers and serializers, you
-- can use the interfaces based on 'B.Builder' as well as @cereal@'s 'G.Get'
-- and 'P.Put' monad.
--
-- The decoders in this module will accept over-long representations.
-- Use "Data.Serialize.LEB128" if you need the strict semantics.
--
-- This code is inspired by Andreas Klebinger's LEB128 implementation in GHC.
module Data.Serialize.LEB128.Lenient
    (
    -- * The class of encodable and decodable types
      LEB128
    , SLEB128
    -- * Bytestring-based interface
    , toLEB128
    , fromLEB128
    , toSLEB128
    , fromSLEB128
    -- * Builder interface
    , buildLEB128
    , buildSLEB128
    -- * Cereal interface
    , getLEB128
    , getSLEB128
    , putLEB128
    , putSLEB128
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Numeric.Natural
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Data.Int
import Data.Maybe
import Data.Monoid ((<>))
import Prelude hiding ((<>))

-- | Unsigned number types can be LEB128-encoded
class (Bits a, Num a, Integral a) => LEB128 a where
instance LEB128 Natural
instance LEB128 Word
instance LEB128 Word8
instance LEB128 Word16
instance LEB128 Word32
instance LEB128 Word64

-- | Signed number types can be SLEB128-encoded
class (Bits a, Num a, Integral a) => SLEB128 a
instance SLEB128 Integer
instance SLEB128 Int
instance SLEB128 Int8
instance SLEB128 Int16
instance SLEB128 Int32
instance SLEB128 Int64

-- | LEB128-encodes a natural number to a strict bytestring
toLEB128 :: LEB128 a => a -> BS.ByteString
toLEB128 = BSL.toStrict . B.toLazyByteStringWith (B.safeStrategy 32 32) BSL.empty . buildLEB128

{-# SPECIALIZE toLEB128 :: Natural -> BS.ByteString #-}
{-# SPECIALIZE toLEB128 :: Word -> BS.ByteString #-}
{-# SPECIALIZE toLEB128 :: Word8 -> BS.ByteString #-}
{-# SPECIALIZE toLEB128 :: Word16 -> BS.ByteString #-}
{-# SPECIALIZE toLEB128 :: Word32 -> BS.ByteString #-}
{-# SPECIALIZE toLEB128 :: Word64 -> BS.ByteString #-}

-- | SLEB128-encodes an integer to a strict bytestring
toSLEB128 :: SLEB128 a => a -> BS.ByteString
toSLEB128 = BSL.toStrict . B.toLazyByteStringWith (B.safeStrategy 32 32) BSL.empty . buildSLEB128

{-# SPECIALIZE toSLEB128 :: Integer -> BS.ByteString #-}
{-# SPECIALIZE toSLEB128 :: Int -> BS.ByteString #-}
{-# SPECIALIZE toSLEB128 :: Int8 -> BS.ByteString #-}
{-# SPECIALIZE toSLEB128 :: Int16 -> BS.ByteString #-}
{-# SPECIALIZE toSLEB128 :: Int32 -> BS.ByteString #-}
{-# SPECIALIZE toSLEB128 :: Int64 -> BS.ByteString #-}

-- | LEB128-encodes a natural number via a builder
buildLEB128 :: LEB128 a => a -> B.Builder
buildLEB128 = go
  where
    go i
      | i <= 127
      = B.word8 (fromIntegral i :: Word8)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        B.word8 (setBit (fromIntegral i) 7) <> go (i `unsafeShiftR` 7)

{-# SPECIALIZE buildLEB128 :: Natural -> B.Builder #-}
{-# SPECIALIZE buildLEB128 :: Word -> B.Builder #-}
{-# SPECIALIZE buildLEB128 :: Word8 -> B.Builder #-}
{-# SPECIALIZE buildLEB128 :: Word16 -> B.Builder #-}
{-# SPECIALIZE buildLEB128 :: Word32 -> B.Builder #-}
{-# SPECIALIZE buildLEB128 :: Word64 -> B.Builder #-}

-- This gets inlined for the specialied variants
isFinite :: forall a. Bits a => Bool
isFinite = isJust (bitSizeMaybe (undefined :: a))

-- | SLEB128-encodes an integer via a builder
buildSLEB128 :: SLEB128 a => a -> B.Builder
buildSLEB128 val
  | val >= -64 && val < 64 = stopByte
  | otherwise = goByte <> buildSLEB128 (shiftR val 7)
  where
  stopByte = B.word8 (fromIntegral $ clearBit val 7)
  goByte = B.word8 (fromIntegral $ setBit val 7)

{-# SPECIALIZE buildSLEB128 :: Integer -> B.Builder #-}
{-# SPECIALIZE buildSLEB128 :: Int -> B.Builder #-}
{-# SPECIALIZE buildSLEB128 :: Int8 -> B.Builder #-}
{-# SPECIALIZE buildSLEB128 :: Int16 -> B.Builder #-}
{-# SPECIALIZE buildSLEB128 :: Int32 -> B.Builder #-}
{-# SPECIALIZE buildSLEB128 :: Int64 -> B.Builder #-}

-- | LEB128-encodes a natural number in @cereal@'s 'P.Put' monad
putLEB128 :: LEB128 a => P.Putter a
putLEB128 = P.putBuilder . buildLEB128
{-# INLINE putLEB128 #-}

-- | SLEB128-encodes an integer in @cereal@'s 'P.Put' monad
putSLEB128 :: SLEB128 a => P.Putter a
putSLEB128 = P.putBuilder . buildSLEB128
{-# INLINE putSLEB128 #-}

-- | LEB128-decodes a natural number from a strict bytestring
fromLEB128 :: LEB128 a => BS.ByteString -> Either String a
fromLEB128 = runComplete getLEB128
{-# INLINE fromLEB128 #-}

-- | SLEB128-decodes an integer from a strict bytestring
fromSLEB128 :: SLEB128 a => BS.ByteString -> Either String a
fromSLEB128 = runComplete getSLEB128
{-# INLINE fromSLEB128 #-}

runComplete :: G.Get a -> BS.ByteString -> Either String a
runComplete p bs = do
    (x,r) <- G.runGetState p bs 0
    unless (BS.null r) $ Left "extra bytes in input"
    return x

-- | LEB128-decodes a natural number via @cereal@
getLEB128 :: forall a. LEB128 a => G.Get a
getLEB128 = G.label "LEB128" $ go 0 0
  where
    go :: Int -> a -> G.Get a
    go !shift !w = do
      byte <- G.getWord8 <|> fail "short encoding"
      let !byteVal = fromIntegral (clearBit byte 7)
      case bitSizeMaybe w of
          Just bs | shift > bs -> fail "overflow"
          _ -> return ()
      when (isFinite @a) $
        unless (byteVal `unsafeShiftL` shift `unsafeShiftR` shift == byteVal) $
          fail "overflow"
      let !val = w .|. (byteVal `unsafeShiftL` shift)
      let !shift' = shift+7
      if hasMore byte
        then go shift' val
        else return $! val

    hasMore b = testBit b 7

{-# SPECIALIZE getLEB128 :: G.Get Natural #-}
{-# SPECIALIZE getLEB128 :: G.Get Word #-}
{-# SPECIALIZE getLEB128 :: G.Get Word8 #-}
{-# SPECIALIZE getLEB128 :: G.Get Word16 #-}
{-# SPECIALIZE getLEB128 :: G.Get Word32 #-}
{-# SPECIALIZE getLEB128 :: G.Get Word64 #-}

-- | SLEB128-decodes an integer via @cereal@
getSLEB128 :: forall a. SLEB128 a => G.Get a
getSLEB128 = G.label "SLEB128" $ go 0 0 0
  where
    go :: Word8 -> Int -> a -> G.Get a
    go !prev !shift !w = do
        byte <- G.getWord8 <|> fail "short encoding"
        let !byteVal = fromIntegral (clearBit byte 7)
        case bitSizeMaybe w of
            Just bs | shift > bs -> fail "overflow"
            _ -> return ()
        when (isFinite @a) $
          unless ((byteVal `unsafeShiftL` shift `unsafeShiftR` shift) .&. 0x7f == byteVal) $
            fail "overflow"
        let !val = w .|. (byteVal `unsafeShiftL` shift)
        let !shift' = shift+7
        if hasMore byte
            then go byte shift' val
            else if signed byte
              then return $! val - bit shift'
              else return $! val

    hasMore b = testBit b 7
    signed b = testBit b 6

{-# SPECIALIZE getSLEB128 :: G.Get Integer #-}
{-# SPECIALIZE getSLEB128 :: G.Get Int #-}
{-# SPECIALIZE getSLEB128 :: G.Get Int8 #-}
{-# SPECIALIZE getSLEB128 :: G.Get Int16 #-}
{-# SPECIALIZE getSLEB128 :: G.Get Int32 #-}
{-# SPECIALIZE getSLEB128 :: G.Get Int64 #-}
