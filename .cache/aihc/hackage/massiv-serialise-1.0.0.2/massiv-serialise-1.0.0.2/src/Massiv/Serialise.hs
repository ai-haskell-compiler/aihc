{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Massiv.Serialise
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
-- This package provides instances for `Serialise` class for all mutable `Array`
-- representations in [@massiv@](https://hackage.haskell.org/package/massiv) package. These
-- instances are provided as orphans from a separate package in order to avoid direct
-- dependency on [@serialise@](https://hackage.haskell.org/package/serialise) package in
-- @massiv@.
--
-- Array serialisation is done by falling back onto instances for `VG.Vector` types from
-- [@vector@](https://hackage.haskell.org/package/vector) package.
--
-- Below is a simple example how to use it. Note a blank module import: @import
-- Massiv.Serialise ()@, which is the only thing needed from this module in order to use
-- provided orphan instances.
--
-- >>> import Massiv.Serialise ()
-- >>> import Data.Massiv.Array as A
-- >>> let arr = A.fromList A.Seq [72,97,115,107,101,108,108] :: A.Vector A.P Int
-- >>> serialise arr
-- "\NUL\a\135\CANH\CANa\CANs\CANk\CANe\CANl\CANl"
-- >>> deserialise (serialise arr) :: A.Vector A.P Int
-- Array P Seq (Sz1 7)
--   [ 72, 97, 115, 107, 101, 108, 108 ]
--
module Massiv.Serialise
  ( -- * Helper functions used to define Serialise instances
    encodeIx
  , decodeIx
  , mkSzFail
  , encodeArray
  , decodeArray
  ) where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.DeepSeq (NFData)
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Foldable as F
import Data.Massiv.Array
import Data.Massiv.Array.Manifest.Vector
import Data.Proxy
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

instance Serialise Comp where
  encode comp =
    case comp of
      Seq      -> encode (0 :: Int)
      ParOn xs -> encode (1 :: Int) <> encode xs
      ParN n   -> encode (2 :: Int) <> encode n
  decode = do
    ty :: Int <- decode
    case ty of
      0 -> pure Seq
      1 -> ParOn <$> decode
      2 -> ParN <$> decode
      n -> Fail.fail $ "Unexpected Comp tag: " <> show n



-- | Encode index
--
-- @since 0.1.0
encodeIx ::
     forall ix. Index ix
  => ix
  -> Encoding
encodeIx = foldlIndex (\ !acc i -> encode i <> acc) mempty

-- | Decode index
--
-- @since 0.1.0
decodeIx ::
     forall s ix. Index ix
  => Decoder s ix
decodeIx = do
  let decodeDim ix dim = do
        i <- decode
        either (Fail.fail . show) pure $! setDimM ix dim i
  F.foldlM decodeDim zeroIndex [1 .. dimensions (Proxy :: Proxy ix)]

instance Serialise Ix2 where
  encode = encodeIx
  decode = decodeIx

instance Index (IxN n) => Serialise (IxN n) where
  encode = encodeIx
  decode = decodeIx



-- | Construct size from index verifying its correctness.
--
-- @since 0.1.0
mkSzFail ::
     forall ix m. (Index ix, Fail.MonadFail m)
  => ix
  -> m (Sz ix)
mkSzFail ix = do
  let guardNegativeOverflow i !acc = do
        when (i < 0) $ Fail.fail $ "Negative size encountered: " <> show i
        let acc' = i * acc
        when (acc' /= 0 && acc' < acc) $ Fail.fail $ "Overflow detected, size is too big: " <> show i
        pure acc'
  Sz ix <$ foldlIndex (\acc i -> acc >>= guardNegativeOverflow i) (pure 1) ix

instance Index ix => Serialise (Sz ix) where
  encode = encodeIx . unSz
  decode = mkSzFail =<< decodeIx



-- | Encode array by using its corresponding vector instance
--
-- @since 0.1.0
encodeArray ::
     forall v r ix e.
     ( Manifest r e
     , Load r ix e
     , Mutable (ARepr v) e
     , VG.Vector v e
     , VRepr (ARepr v) ~ v
     , Serialise (v e)
     )
  => Array r ix e
  -> Encoding
encodeArray arr =
  encode (getComp arr) <> encode (size arr) <> encode (toVector arr :: v e)

-- | Decode array by using its corresponding vector instance
--
-- @since 0.1.0
decodeArray ::
     forall v r ix e s.
     ( Typeable v
     , VG.Vector v e
     , Load r ix e
     , Load (ARepr v) ix e
     , Mutable r e
     , Serialise (v e)
     )
  => Decoder s (Array r ix e)
decodeArray = do
  comp <- decode
  sz <- decode
  vector :: v e <- decode
  either (Fail.fail . show) pure $ fromVectorM comp sz vector

instance (Index ix, Serialise e) => Serialise (Array BL ix e) where
  encode = encodeArray @V.Vector
  decode = decodeArray @V.Vector

instance (Index ix, Serialise e) => Serialise (Array B ix e) where
  encode = encode . toLazyArray
  decode = evalLazyArray <$> decode

instance (Index ix, NFData e, Serialise e) => Serialise (Array BN ix e) where
  encode = encode . unwrapNormalForm
  decode = forceLazyArray <$> decode

instance (Index ix, Storable e, Serialise e) => Serialise (Array S ix e) where
  encode = encodeArray @VS.Vector
  decode = decodeArray @VS.Vector

instance (Index ix, Unbox e, Serialise e) => Serialise (Array U ix e) where
  encode = encodeArray @VU.Vector
  decode = decodeArray @VU.Vector

instance (Index ix, Prim e, Serialise e) => Serialise (Array P ix e) where
  encode = encodeArray @VP.Vector
  decode = decodeArray @VP.Vector

