{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains internal test harnesses for `base32`
--
module Internal
( Harness(..)
, TextHarness(..)
, b32
, lb32
, sb32
, t32
, tl32
, ts32
, tt32
, ttl32
, tts32
) where


import Test.QuickCheck hiding (label)
import Data.Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base32 as B32
import qualified Data.ByteString.Base32.Hex as B32H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Base32 as BL32
import qualified Data.ByteString.Lazy.Base32.Hex as BL32H
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Base32 as BS32
import qualified Data.ByteString.Short.Base32.Hex as BS32H
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base32 as T32
import qualified Data.Text.Encoding.Base32.Hex as T32H
import Data.Text.Encoding.Base32.Error
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding.Base32 as TL32
import qualified Data.Text.Lazy.Encoding.Base32.Hex as TL32H
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Encoding.Base32 as TS32
import qualified Data.Text.Short.Encoding.Base32.Hex as TS32H

-- ------------------------------------------------------------------ --
-- Test Harnesses



-- | This class provides the generic API definition for
-- the base32 std alphabet
--
data Harness bs = Harness
  { label :: String
  , encode :: bs -> bs
  , encodeNopad :: bs -> bs
  , encodeHex :: bs -> bs
  , encodeHexNopad :: bs -> bs
  , decode :: bs -> Either Text bs
  , decodeHex :: bs -> Either Text bs
  , decodePad :: bs -> Either Text bs
  , decodeHexPad :: bs -> Either Text bs
  , decodeNopad :: bs -> Either Text bs
  , decodeHexNopad :: bs -> Either Text bs
  , correct :: bs -> Bool
  , correctHex :: bs -> Bool
  , validate :: bs -> Bool
  , validateHex :: bs -> Bool
  }

b32 :: Harness BS.ByteString
b32 = Harness
  { label = "ByteString"
  , encode = B32.encodeBase32'
  , encodeNopad = B32.encodeBase32Unpadded'
  , decode = B32.decodeBase32
  , decodePad = B32.decodeBase32Padded
  , decodeNopad = B32.decodeBase32Unpadded
  , correct = B32.isBase32
  , validate = B32.isValidBase32
  , encodeHex = B32H.encodeBase32'
  , encodeHexNopad = B32H.encodeBase32Unpadded'
  , decodeHex = B32H.decodeBase32
  , decodeHexPad = B32H.decodeBase32Padded
  , decodeHexNopad = B32H.decodeBase32Unpadded
  , correctHex = B32H.isBase32Hex
  , validateHex = B32H.isValidBase32Hex
  }

lb32 :: Harness LBS.ByteString
lb32 = Harness
  { label = "Lazy ByteString"
  , encode = BL32.encodeBase32'
  , encodeNopad = BL32.encodeBase32Unpadded'
  , decode = BL32.decodeBase32
  , decodePad = BL32.decodeBase32Padded
  , decodeNopad = BL32.decodeBase32Unpadded
  , correct = BL32.isBase32
  , validate = BL32.isValidBase32
  , encodeHex = BL32H.encodeBase32'
  , encodeHexNopad = BL32H.encodeBase32Unpadded'
  , decodeHex = BL32H.decodeBase32
  , decodeHexPad = BL32H.decodeBase32Padded
  , decodeHexNopad = BL32H.decodeBase32Unpadded
  , correctHex = BL32H.isBase32Hex
  , validateHex = BL32H.isValidBase32Hex
  }

sb32 :: Harness SBS.ShortByteString
sb32 = Harness
  { label = "Short ByteString"
  , encode = BS32.encodeBase32'
  , encodeNopad = BS32.encodeBase32Unpadded'
  , decode = BS32.decodeBase32
  , decodePad = BS32.decodeBase32Padded
  , decodeNopad = BS32.decodeBase32Unpadded
  , correct = BS32.isBase32
  , validate = BS32.isValidBase32
  , encodeHex = BS32H.encodeBase32'
  , encodeHexNopad = BS32H.encodeBase32Unpadded'
  , decodeHex = BS32H.decodeBase32
  , decodeHexPad = BS32H.decodeBase32Padded
  , decodeHexNopad = BS32H.decodeBase32Unpadded
  , correctHex = BS32H.isBase32Hex
  , validateHex = BS32H.isValidBase32Hex
  }

t32 :: Harness T.Text
t32 = Harness
  { label = "Text"
  , encode = T32.encodeBase32
  , encodeNopad = T32.encodeBase32Unpadded
  , decode = T32.decodeBase32
  , decodePad = T32.decodeBase32Padded
  , decodeNopad = T32.decodeBase32Unpadded
  , correct = T32.isBase32
  , validate = T32.isValidBase32
  , encodeHex = T32H.encodeBase32
  , encodeHexNopad = T32H.encodeBase32Unpadded
  , decodeHex = T32H.decodeBase32
  , decodeHexPad = T32H.decodeBase32Padded
  , decodeHexNopad = T32H.decodeBase32Unpadded
  , correctHex = T32H.isBase32Hex
  , validateHex = T32H.isValidBase32Hex
  }

tl32 :: Harness TL.Text
tl32 = Harness
  { label = "Lazy Text"
  , encode = TL32.encodeBase32
  , encodeNopad = TL32.encodeBase32Unpadded
  , decode = TL32.decodeBase32
  , decodePad = TL32.decodeBase32Padded
  , decodeNopad = TL32.decodeBase32Unpadded
  , correct = TL32.isBase32
  , validate = TL32.isValidBase32
  , encodeHex = TL32H.encodeBase32
  , encodeHexNopad = TL32H.encodeBase32Unpadded
  , decodeHex = TL32H.decodeBase32
  , decodeHexPad = TL32H.decodeBase32Padded
  , decodeHexNopad = TL32H.decodeBase32Unpadded
  , correctHex = TL32H.isBase32Hex
  , validateHex = TL32H.isValidBase32Hex
  }

ts32 :: Harness TS.ShortText
ts32 = Harness
  { label = "Short Lazy Text"
  , encode = TS32.encodeBase32
  , encodeNopad = TS32.encodeBase32Unpadded
  , decode = TS32.decodeBase32
  , decodePad = TS32.decodeBase32Padded
  , decodeNopad = TS32.decodeBase32Unpadded
  , correct = TS32.isBase32
  , validate = TS32.isValidBase32
  , encodeHex = TS32H.encodeBase32
  , encodeHexNopad = TS32H.encodeBase32Unpadded
  , decodeHex = TS32H.decodeBase32
  , decodeHexPad = TS32H.decodeBase32Padded
  , decodeHexNopad = TS32H.decodeBase32Unpadded
  , correctHex = TS32H.isBase32Hex
  , validateHex = TS32H.isValidBase32Hex
  }

-- -------------------------------------------------------------------- --
-- Text-specific harness

data TextHarness bs cs = TextHarness
  { decodeWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodePaddedWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeUnpaddedWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeHexWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeHexPaddedWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  , decodeHexUnpaddedWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base32Error err) cs
  }

tt32 :: TextHarness BS.ByteString Text
tt32 = TextHarness
  { decodeWith_ = T32.decodeBase32With
  , decodePaddedWith_ = T32.decodeBase32PaddedWith
  , decodeUnpaddedWith_ = T32.decodeBase32UnpaddedWith
  , decodeHexWith_ = T32H.decodeBase32With
  , decodeHexPaddedWith_ = T32H.decodeBase32PaddedWith
  , decodeHexUnpaddedWith_ = T32H.decodeBase32UnpaddedWith
  }

ttl32 :: TextHarness LBS.ByteString TL.Text
ttl32 = TextHarness
  { decodeWith_ = TL32.decodeBase32With
  , decodePaddedWith_ = TL32.decodeBase32PaddedWith
  , decodeUnpaddedWith_ = TL32.decodeBase32UnpaddedWith
  , decodeHexWith_ = TL32H.decodeBase32With
  , decodeHexPaddedWith_ = TL32H.decodeBase32PaddedWith
  , decodeHexUnpaddedWith_ = TL32H.decodeBase32UnpaddedWith
  }

tts32 :: TextHarness SBS.ShortByteString TS.ShortText
tts32 = TextHarness
  { decodeWith_ = TS32.decodeBase32With
  , decodePaddedWith_ = TS32.decodeBase32PaddedWith
  , decodeUnpaddedWith_ = TS32.decodeBase32UnpaddedWith
  , decodeHexWith_ = TS32H.decodeBase32With
  , decodeHexPaddedWith_ = TS32H.decodeBase32PaddedWith
  , decodeHexUnpaddedWith_ = TS32H.decodeBase32UnpaddedWith
  }

-- ------------------------------------------------------------------ --
-- Quickcheck instances

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance Arbitrary LBS.ByteString where
    arbitrary = LBS.pack <$> arbitrary
    shrink xs = LBS.pack <$> shrink (LBS.unpack xs)

instance CoArbitrary LBS.ByteString where
    coarbitrary = coarbitrary . LBS.unpack

instance Arbitrary SBS.ShortByteString where
    arbitrary = SBS.pack <$> arbitrary
    shrink xs = SBS.pack <$> shrink (SBS.unpack xs)

instance CoArbitrary SBS.ShortByteString where
    coarbitrary = coarbitrary . SBS.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack . getUnicodeString <$> arbitrary
    shrink xs = T.pack . getUnicodeString <$> shrink (UnicodeString $ T.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack . getUnicodeString <$> arbitrary
    shrink xs = TL.pack . getUnicodeString <$> shrink (UnicodeString $ TL.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack

instance Arbitrary TS.ShortText where
  arbitrary = TS.fromText <$> arbitrary
  shrink xs = TS.fromText <$> shrink (TS.toText xs)

instance CoArbitrary TS.ShortText where
  coarbitrary = coarbitrary . TS.toText
