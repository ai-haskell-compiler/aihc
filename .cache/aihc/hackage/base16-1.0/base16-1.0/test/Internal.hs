{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2020-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains internal test harnesses for `base16`
--
module Internal where


import Data.Base16.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Short as SBS
import "base16" Data.ByteString.Base16 as B16
import "base16" Data.ByteString.Lazy.Base16 as LB16
import "base16" Data.ByteString.Short.Base16 as SB16
import Data.Char (toLower)
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import "base16" Data.Text.Encoding.Base16 as T16
import Data.Text.Encoding.Base16.Error (Base16Error(..))
import qualified Data.Text.Lazy as TL
import "base16" Data.Text.Lazy.Encoding.Base16 as TL16
import qualified Data.Text.Short as TS
import "base16" Data.Text.Short.Encoding.Base16 as TS16

import Test.QuickCheck hiding (label)

-- ------------------------------------------------------------------ --
-- Test Harnesses

data Impl
  = B16
  | LB16
  | SB16
  | T16
  | TL16
  | TS16

b16 :: Proxy 'B16
b16 = Proxy

lb16 :: Proxy 'LB16
lb16 = Proxy

sb16 :: Proxy 'SB16
sb16 = Proxy

t16 :: Proxy 'T16
t16 = Proxy

tl16 :: Proxy 'TL16
tl16 = Proxy

ts16 :: Proxy 'TS16
ts16 = Proxy

-- | This class provides the generic API definition for
-- the base16 std alphabet
--
class
  ( Eq bs
  , Show bs
  , Arbitrary bs
  , CoArbitrary bs
  , IsString bs
  ) => Harness (a :: Impl) bs | a -> bs, bs -> a
  where

  label :: String
  encode :: bs -> Base16 bs
  decodeTyped :: Base16 bs -> bs
  decode :: bs -> Either Text bs
  lenient :: bs -> bs

  lower :: bs -> bs
  correct :: bs -> Bool
  validate :: bs -> Bool


instance Harness 'B16 BS.ByteString where
  label = "ByteString"

  encode = B16.encodeBase16'
  decodeTyped = B16.decodeBase16
  decode = B16.decodeBase16Untyped
  lenient = B16.decodeBase16Lenient
  correct = B16.isBase16
  validate = B16.isValidBase16
  lower = BS8.map toLower

instance Harness 'LB16 LBS.ByteString where
  label = "Lazy ByteString"

  encode = LB16.encodeBase16'
  decodeTyped = LB16.decodeBase16
  decode = LB16.decodeBase16Untyped
  lenient = LB16.decodeBase16Lenient
  correct = LB16.isBase16
  validate = LB16.isValidBase16
  lower = LBS8.map toLower

instance Harness 'SB16 SBS.ShortByteString where
  label = "Short ByteString"

  encode = SB16.encodeBase16'
  decodeTyped = SB16.decodeBase16
  decode = SB16.decodeBase16Untyped
  lenient = SB16.decodeBase16Lenient
  correct = SB16.isBase16
  validate = SB16.isValidBase16
  lower = SBS.toShort . BS8.map toLower . SBS.fromShort

instance Harness 'T16 Text where
  label = "Text"

  encode = T16.encodeBase16
  decodeTyped = T16.decodeBase16
  decode = T16.decodeBase16Untyped
  lenient = T16.decodeBase16Lenient
  correct = T16.isBase16
  validate = T16.isValidBase16
  lower = T.map toLower

instance Harness 'TL16 TL.Text where
  label = "Lazy Text"

  encode = TL16.encodeBase16
  decodeTyped = TL16.decodeBase16
  decode = TL16.decodeBase16Untyped
  lenient = TL16.decodeBase16Lenient
  correct = TL16.isBase16
  validate = TL16.isValidBase16
  lower = TL.map toLower

instance Harness 'TS16 TS.ShortText where
  label = "Short Text"

  encode = TS16.encodeBase16
  decodeTyped = TS16.decodeBase16
  decode = TS16.decodeBase16Untyped
  lenient = TS16.decodeBase16Lenient
  correct = TS16.isBase16
  validate = TS16.isValidBase16
  lower = TS.fromText . T.map toLower . TS.toText

class Harness a cs
  => TextHarness (a :: Impl) cs bs
  | a -> cs, bs -> cs, cs -> a, cs -> bs where
  decodeWith_ :: (bs -> Either err cs) -> bs -> Either (Base16Error err) cs

instance TextHarness 'T16 Text BS.ByteString where
  decodeWith_ = T16.decodeBase16With

instance TextHarness 'TL16 TL.Text LBS.ByteString where
  decodeWith_ = TL16.decodeBase16With

instance TextHarness 'TS16 TS.ShortText SBS.ShortByteString where
  decodeWith_ = TS16.decodeBase16With

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
