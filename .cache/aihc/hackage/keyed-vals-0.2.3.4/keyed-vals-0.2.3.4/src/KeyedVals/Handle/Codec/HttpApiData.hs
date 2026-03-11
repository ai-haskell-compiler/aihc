{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module KeyedVals.Handle.Codec.HttpApiData (
  -- * newtypes
  HttpApiDataOf (..),
) where

import KeyedVals.Handle.Codec (DecodeKV (..), EncodeKV (..))
import Web.HttpApiData (
  FromHttpApiData (..),
  ToHttpApiData (..),
 )


{- | A deriving-via helper type for types that implement 'DecodeKV' and 'EncodeKV'
 using 'FromHttpApiData' and 'ToHttpApiData' type classes.
-}
newtype HttpApiDataOf a = HttpApiDataOf {fromHttpApiDataOf :: a}


instance FromHttpApiData a => DecodeKV (HttpApiDataOf a) where
  decodeKV = fmap HttpApiDataOf . parseHeader


instance ToHttpApiData a => EncodeKV (HttpApiDataOf a) where
  encodeKV = toHeader . fromHttpApiDataOf
