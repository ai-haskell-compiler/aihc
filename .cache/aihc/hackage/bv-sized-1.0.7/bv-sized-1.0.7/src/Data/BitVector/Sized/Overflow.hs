{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Data.BitVector.Sized.Overflow
Copyright   : (c) Galois Inc. 2020
License     : BSD-3
Maintainer  : Ben Selfridge <benselfridge@galois.com>
Stability   : experimental
Portability : portable

This module provides alternative definitions of certain bitvector
functions that might produce signed or unsigned overflow. Instead of
producing a pure value, these versions produce the same value along
with overflow flags. We only provide definitions for operators that
might actually overflow.

-}

module Data.BitVector.Sized.Overflow
  ( Overflow(..)
  , UnsignedOverflow(..)
  , SignedOverflow(..)
  , ofUnsigned
  , ofSigned
  , ofResult
  -- * Overflowing bitwise operators
  , shlOf
  -- * Overflowing arithmetic operators
  , addOf
  , subOf
  , mulOf
  , squotOf
  , sremOf
  , sdivOf
  , smodOf
  ) where

import qualified Data.Bits as B
import Numeric.Natural
import GHC.TypeLits

import Data.Parameterized ( NatRepr )
import qualified Data.Parameterized.NatRepr as P

import Data.BitVector.Sized.Internal ( BV(..)
                                     , mkBV'
                                     , asUnsigned
                                     , asSigned
                                     , shiftAmount
                                     )


----------------------------------------
-- Unsigned and signed overflow datatypes

-- | Datatype representing the possibility of unsigned overflow.
data UnsignedOverflow = UnsignedOverflow
                      | NoUnsignedOverflow
  deriving (Show, Eq)

instance Semigroup UnsignedOverflow where
  NoUnsignedOverflow <> NoUnsignedOverflow = NoUnsignedOverflow
  _ <> _ = UnsignedOverflow

instance Monoid UnsignedOverflow where
  mempty = NoUnsignedOverflow

-- | Datatype representing the possibility of signed overflow.
data SignedOverflow = SignedOverflow
                    | NoSignedOverflow
  deriving (Show, Eq)

instance Semigroup SignedOverflow where
  NoSignedOverflow <> NoSignedOverflow = NoSignedOverflow
  _ <> _ = SignedOverflow

instance Monoid SignedOverflow where
  mempty = NoSignedOverflow

----------------------------------------
-- Overflow wrapper
-- | A value annotated with overflow information.
data Overflow a =
  Overflow UnsignedOverflow SignedOverflow a
  deriving (Show, Eq)

-- | Return 'True' if a computation caused unsigned overflow.
ofUnsigned :: Overflow a -> Bool
ofUnsigned (Overflow UnsignedOverflow _ _) = True
ofUnsigned _ = False

-- | Return 'True' if a computation caused signed overflow.
ofSigned :: Overflow a -> Bool
ofSigned (Overflow _ SignedOverflow _) = True
ofSigned _ = False

-- | Return the result of a computation.
ofResult :: Overflow a -> a
ofResult (Overflow _ _ res) = res

instance Foldable Overflow where
  foldMap f (Overflow _ _ a) = f a

instance Traversable Overflow where
  sequenceA (Overflow uof sof a) = Overflow uof sof <$> a

instance Functor Overflow where
  fmap f (Overflow uof sof a) = Overflow uof sof (f a)

instance Applicative Overflow where
  pure a = Overflow mempty mempty a
  Overflow uof sof f <*> Overflow uof' sof' a =
    Overflow (uof <> uof') (sof <> sof') (f a)

-- | Monad for bitvector operations which might produce signed or
-- unsigned overflow.
instance Monad Overflow where
  Overflow uof sof a >>= k =
    let Overflow uof' sof' b = k a
    in Overflow (uof <> uof') (sof <> sof') b

getUof :: NatRepr w -> Integer -> UnsignedOverflow
getUof w x = if x < P.minUnsigned w || x > P.maxUnsigned w
             then UnsignedOverflow
             else NoUnsignedOverflow

getSof :: NatRepr w -> Integer -> SignedOverflow
getSof w x = case P.isZeroOrGT1 w of
  Left P.Refl -> NoSignedOverflow
  Right P.LeqProof ->
    if x < P.minSigned w || x > P.maxSigned w
    then SignedOverflow
    else NoSignedOverflow

-- | This only works if the operation has equivalent signed and
-- unsigned interpretations on bitvectors.
liftBinary :: (1 <= w) => (Integer -> Integer -> Integer)
           -> NatRepr w
           -> BV w -> BV w -> Overflow (BV w)
liftBinary op w xv yv =
  let ux = asUnsigned xv
      uy = asUnsigned yv
      sx = asSigned w xv
      sy = asSigned w yv

      ures = ux `op` uy
      sres = sx `op` sy

      uof = getUof w ures
      sof = getSof w sres
  in Overflow uof sof (mkBV' w ures)

-- | Bitvector add.
addOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
addOf = liftBinary (+)

-- | Bitvector subtract.
subOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
subOf = liftBinary (-)

-- | Bitvector multiply.
mulOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
mulOf = liftBinary (*)

-- | Left shift by positive 'Natural'.
shlOf :: (1 <= w) => NatRepr w -> BV w -> Natural -> Overflow (BV w)
shlOf w xv shf =
  let ux = asUnsigned xv
      sx = asSigned w xv
      ures = ux `B.shiftL` shiftAmount w shf
      sres = sx `B.shiftL` shiftAmount w shf
      uof = getUof w ures
      sof = getSof w sres
  in Overflow uof sof (mkBV' w ures)

-- | Bitvector division (signed). Rounds to zero. Division by zero
-- yields a runtime error.
squotOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
squotOf w bv1 bv2 = Overflow NoUnsignedOverflow sof (mkBV' w (x `quot` y))
  where x = asSigned w bv1
        y = asSigned w bv2
        sof = if (x == P.minSigned w && y == -1)
              then SignedOverflow
              else NoSignedOverflow

-- | Bitvector remainder after division (signed), when rounded to
-- zero. Division by zero yields a runtime error.
sremOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
sremOf w bv1 bv2 = Overflow NoUnsignedOverflow sof (mkBV' w (x `rem` y))
  where x = asSigned w bv1
        y = asSigned w bv2
        sof = if (x == P.minSigned w && y == -1)
              then SignedOverflow
              else NoSignedOverflow

-- | Bitvector division (signed). Rounds to zero. Division by zero
-- yields a runtime error.
sdivOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
sdivOf w bv1 bv2 = Overflow NoUnsignedOverflow sof (mkBV' w (x `div` y))
  where x = asSigned w bv1
        y = asSigned w bv2
        sof = if (x == P.minSigned w && y == -1)
              then SignedOverflow
              else NoSignedOverflow

-- | Bitvector remainder after division (signed), when rounded to
-- zero. Division by zero yields a runtime error.
smodOf :: (1 <= w) => NatRepr w -> BV w -> BV w -> Overflow (BV w)
smodOf w bv1 bv2 = Overflow NoUnsignedOverflow sof (mkBV' w (x `mod` y))
  where x = asSigned w bv1
        y = asSigned w bv2
        sof = if (x == P.minSigned w && y == -1)
              then SignedOverflow
              else NoSignedOverflow
