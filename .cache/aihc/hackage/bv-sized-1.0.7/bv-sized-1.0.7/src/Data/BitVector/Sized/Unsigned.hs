{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Data.BitVector.Sized.Unsigned
Copyright   : (c) Galois Inc. 2018
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module defines a wrapper around the 'BV' type, 'UnsignedBV', with
instances not provided by 'BV'.
-}

module Data.BitVector.Sized.Unsigned
  ( UnsignedBV(..)
  , mkUnsignedBV
  ) where

import Control.DeepSeq (NFData)
import           Data.BitVector.Sized.Internal (BV(..), mkBV)
import qualified Data.BitVector.Sized.Internal as BV
import           Data.BitVector.Sized.Panic (panic)
import           Data.Parameterized.Classes (Hashable(..))
import           Data.Parameterized.NatRepr (NatRepr, knownNat, maxUnsigned, widthVal)

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Ix (Ix(inRange, range, index))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat)
import Language.Haskell.TH.Lift (Lift)
import Numeric.Natural (Natural)
import System.Random
import System.Random.Stateful

-- | Signed bit vector.
newtype UnsignedBV w = UnsignedBV { asBV :: BV w }
  deriving (Generic, Show, Read, Eq, Ord, Lift, NFData)

-- | Convenience wrapper for 'BV.mkBV'.
mkUnsignedBV :: NatRepr w -> Integer -> UnsignedBV w
mkUnsignedBV w x = UnsignedBV (BV.mkBV w x)

liftUnary :: (BV w -> BV w)
          -> UnsignedBV w
          -> UnsignedBV w
liftUnary op (UnsignedBV bv) = UnsignedBV (op bv)

liftBinary :: (BV w -> BV w -> BV w)
           -> UnsignedBV w
           -> UnsignedBV w
           -> UnsignedBV w
liftBinary op (UnsignedBV bv1) (UnsignedBV bv2) = UnsignedBV (op bv1 bv2)

liftBinaryInt :: (BV w -> Natural -> BV w)
              -> UnsignedBV w
              -> Int
              -> UnsignedBV w
liftBinaryInt op (UnsignedBV bv) i = UnsignedBV (op bv (intToNatural i))

intToNatural :: Int -> Natural
intToNatural = fromIntegral

instance KnownNat w => Bits (UnsignedBV w) where
  (.&.)        = liftBinary BV.and
  (.|.)        = liftBinary BV.or
  xor          = liftBinary BV.xor
  complement   = liftUnary (BV.complement knownNat)
  shiftL       = liftBinaryInt (BV.shl knownNat)
  shiftR       = liftBinaryInt (BV.lshr knownNat)
  rotateL      = liftBinaryInt (BV.rotateL knownNat)
  rotateR      = liftBinaryInt (BV.rotateR knownNat)
  bitSize _    = widthVal (knownNat @w)
  bitSizeMaybe _ = Just (widthVal (knownNat @w))
  isSigned     = const False
  testBit (UnsignedBV bv) ix = BV.testBit' (intToNatural ix) bv
  bit          = UnsignedBV . BV.bit' knownNat . intToNatural
  popCount (UnsignedBV bv) = fromInteger (BV.asUnsigned (BV.popCount bv))

instance KnownNat w => FiniteBits (UnsignedBV w) where
  finiteBitSize _ = widthVal (knownNat @w)
  countLeadingZeros  (UnsignedBV bv) = fromInteger $ BV.asUnsigned $ BV.clz knownNat bv
  countTrailingZeros (UnsignedBV bv) = fromInteger $ BV.asUnsigned $ BV.ctz knownNat bv

instance KnownNat w => Num (UnsignedBV w) where
  (+)         = liftBinary (BV.add knownNat)
  (*)         = liftBinary (BV.mul knownNat)
  abs         = id
  signum (UnsignedBV bv) = UnsignedBV $ BV.BV $ signum $ BV.asUnsigned bv
  fromInteger = UnsignedBV . mkBV knownNat
  -- in this case, negate just means "additive inverse"
  negate      = liftUnary (BV.negate knownNat)

instance KnownNat w => Enum (UnsignedBV w) where
  toEnum = UnsignedBV . mkBV knownNat . checkInt
    where checkInt i | 0 <= i && toInteger i <= maxUnsigned (knownNat @w) = toInteger i
                     | otherwise = panic "Data.BitVector.Sized.Unsigned"
                                   ["toEnum: bad argument"]

  fromEnum (UnsignedBV bv) = fromInteger (BV.asUnsigned bv)

instance KnownNat w => Ix (UnsignedBV w) where
  range (UnsignedBV loBV, UnsignedBV hiBV) =
    UnsignedBV . mkBV knownNat <$>
    [BV.asUnsigned loBV .. BV.asUnsigned hiBV]
  index (UnsignedBV loBV, UnsignedBV hiBV) (UnsignedBV ixBV) =
    index ( BV.asUnsigned loBV,
            BV.asUnsigned hiBV)
    (BV.asUnsigned ixBV)
  inRange (UnsignedBV loBV, UnsignedBV hiBV) (UnsignedBV ixBV) =
    inRange ( BV.asUnsigned loBV
            , BV.asUnsigned hiBV)
    (BV.asUnsigned ixBV)

instance KnownNat w => Bounded (UnsignedBV w) where
  minBound = UnsignedBV (BV.minUnsigned knownNat)
  maxBound = UnsignedBV (BV.maxUnsigned knownNat)

instance KnownNat w => Uniform (UnsignedBV w) where
  uniformM g = UnsignedBV <$> BV.uniformM knownNat g

instance UniformRange (UnsignedBV w) where
  uniformRM (UnsignedBV lo, UnsignedBV hi) g =
    UnsignedBV <$> BV.uUniformRM (lo, hi) g
#if MIN_VERSION_random(1,3,0)
  isInRange (lo, hi) x =
    lo <= x && x <= hi
#endif

instance KnownNat w => Random (UnsignedBV w)

instance Hashable (UnsignedBV w) where
  hashWithSalt salt (UnsignedBV b) = hashWithSalt salt b
