{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.BitVector.Sized.Signed
Copyright   : (c) Galois Inc. 2018
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a wrapper around the 'BV' type, 'SignedBV', with
instances not provided by 'BV'.
-}

module Data.BitVector.Sized.Signed
  ( SignedBV(..)
  , mkSignedBV
  ) where

import Control.DeepSeq (NFData)
import           Data.BitVector.Sized (BV, mkBV)
import qualified Data.BitVector.Sized.Internal as BV
import           Data.BitVector.Sized.Panic (panic)
import Data.Parameterized.Classes (Hashable(..))
import Data.Parameterized.NatRepr

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Ix
import GHC.Generics
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH.Lift (Lift)
import Numeric.Natural (Natural)
import System.Random
import System.Random.Stateful

-- | Signed bit vector.
newtype SignedBV w = SignedBV { asBV :: BV w }
  deriving (Generic, Show, Read, Eq, Lift, NFData)

instance (KnownNat w, 1 <= w) => Ord (SignedBV w) where
  SignedBV bv1 `compare` SignedBV bv2 =
    if | bv1 == bv2              -> EQ
       | BV.slt knownNat bv1 bv2 -> LT
       | otherwise               -> GT

-- | Convenience wrapper for 'BV.mkBV'.
mkSignedBV :: NatRepr w -> Integer -> SignedBV w
mkSignedBV w x = SignedBV (BV.mkBV w x)

liftUnary :: (BV w -> BV w)
          -> SignedBV w
          -> SignedBV w
liftUnary op (SignedBV bv) = SignedBV (op bv)

liftBinary :: (BV w -> BV w -> BV w)
           -> SignedBV w
           -> SignedBV w
           -> SignedBV w
liftBinary op (SignedBV bv1) (SignedBV bv2) = SignedBV (op bv1 bv2)

liftBinaryInt :: (BV w -> Natural -> BV w)
              -> SignedBV w
              -> Int
              -> SignedBV w
liftBinaryInt op (SignedBV bv) i = SignedBV (op bv (intToNatural i))

intToNatural :: Int -> Natural
intToNatural = fromIntegral

instance (KnownNat w, 1 <= w) => Bits (SignedBV w) where
  (.&.)        = liftBinary BV.and
  (.|.)        = liftBinary BV.or
  xor          = liftBinary BV.xor
  complement   = liftUnary (BV.complement knownNat)
  shiftL       = liftBinaryInt (BV.shl knownNat)
  shiftR       = liftBinaryInt (BV.ashr knownNat)
  rotateL      = liftBinaryInt (BV.rotateL knownNat)
  rotateR      = liftBinaryInt (BV.rotateR knownNat)
  bitSize _    = widthVal (knownNat @w)
  bitSizeMaybe _ = Just (widthVal (knownNat @w))
  isSigned     = const True
  testBit (SignedBV bv) ix = BV.testBit' (intToNatural ix) bv
  bit          = SignedBV . BV.bit' knownNat . intToNatural
  popCount (SignedBV bv) = fromInteger (BV.asUnsigned (BV.popCount bv))

instance (KnownNat w, 1 <= w) => FiniteBits (SignedBV w) where
  finiteBitSize _ = widthVal (knownNat @w)
  countLeadingZeros  (SignedBV bv) = fromInteger $ BV.asUnsigned $ BV.clz knownNat bv
  countTrailingZeros (SignedBV bv) = fromInteger $ BV.asUnsigned $ BV.ctz knownNat bv

instance (KnownNat w, 1 <= w) => Num (SignedBV w) where
  (+)         = liftBinary (BV.add knownNat)
  (*)         = liftBinary (BV.mul knownNat)
  abs         = liftUnary (BV.abs knownNat)
  signum (SignedBV bv) = mkSignedBV knownNat $ signum $ BV.asSigned knownNat bv
  fromInteger = SignedBV . mkBV knownNat
  negate      = liftUnary (BV.negate knownNat)

instance (KnownNat w, 1 <= w) => Enum (SignedBV w) where
  toEnum = SignedBV . mkBV knownNat . checkInt
    where checkInt i | lo <= toInteger i && toInteger i <= hi = toInteger i
                     | otherwise = panic "Data.BitVector.Sized.Signed"
                                   ["toEnum: bad argument"]
            where lo = minSigned (knownNat @w)
                  hi = maxSigned (knownNat @w)

  fromEnum (SignedBV bv) = fromInteger (BV.asSigned (knownNat @w) bv)

instance (KnownNat w, 1 <= w) => Ix (SignedBV w) where
  range (SignedBV loBV, SignedBV hiBV) =
    (SignedBV . mkBV knownNat) <$>
    [BV.asSigned knownNat loBV .. BV.asSigned knownNat hiBV]
  index (SignedBV loBV, SignedBV hiBV) (SignedBV ixBV) =
    index ( BV.asSigned knownNat loBV
          , BV.asSigned knownNat hiBV)
    (BV.asSigned knownNat ixBV)
  inRange (SignedBV loBV, SignedBV hiBV) (SignedBV ixBV) =
    inRange ( BV.asSigned knownNat loBV
            , BV.asSigned knownNat hiBV)
    (BV.asSigned knownNat ixBV)

instance (KnownNat w, 1 <= w) => Bounded (SignedBV w) where
  minBound = SignedBV (BV.minSigned knownNat)
  maxBound = SignedBV (BV.maxSigned knownNat)

instance KnownNat w => Uniform (SignedBV w) where
  uniformM g = SignedBV <$> BV.uniformM knownNat g

instance (KnownNat w, 1 <= w) => UniformRange (SignedBV w) where
  uniformRM (SignedBV lo, SignedBV hi) g =
    SignedBV <$> BV.sUniformRM knownNat (lo, hi) g
#if MIN_VERSION_random(1,3,0)
  isInRange (lo, hi) x =
    lo <= x && x <= hi
#endif

instance (KnownNat w, 1 <= w) => Random (SignedBV w)

instance Hashable (SignedBV w) where
  hashWithSalt salt (SignedBV b) = hashWithSalt salt b
