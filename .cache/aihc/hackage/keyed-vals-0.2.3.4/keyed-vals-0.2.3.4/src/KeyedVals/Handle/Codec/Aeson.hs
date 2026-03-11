{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module KeyedVals.Handle.Codec.Aeson (
  -- * newtypes
  AesonOf (..),
) where

import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  eitherDecodeStrict',
  encode,
 )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import KeyedVals.Handle.Codec (DecodeKV (..), EncodeKV (..))


{- | A deriving-via helper type for types that implement 'DecodeKV' and 'EncodeKV'
 using Aeson's 'FromJSON' and 'ToJSON' type classes.
-}
newtype AesonOf a = AesonOf {fromAesonOf :: a}


instance FromJSON a => DecodeKV (AesonOf a) where
  decodeKV = either (Left . Text.pack) (Right . AesonOf) . eitherDecodeStrict'


instance ToJSON a => EncodeKV (AesonOf a) where
  encodeKV = LBS.toStrict . encode . fromAesonOf
