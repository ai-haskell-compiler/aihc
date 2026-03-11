{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------
-- |
-- Module      :  Data.Boolean.Numbers
-- Copyright   :  (c) Jan Bracker 2013
-- License     :  BSD3
-- 
-- Maintainer  :  jbra@informatik.uni-kiel.de
-- Stability   :  experimental
-- 
-- A generalized version of the class hirarchy for numbers. All
-- functions that would break a potential deep embedding are removed
-- or generalized to support deep embeddings.
-- 
-- The class hierarchy for numeric types keeps as close as possible to the 
-- 'Prelude' hierarchy. A great part of the default implementation and comments
-- are copied and adopted from 'Prelude'.
--
-------------------------------------------------------------------------

module Data.Boolean.Numbers 
  ( NumB(..)
  , IntegralB(..)
  , RealFracB(..)
  , RealFloatB(..)
  , evenB, oddB
  , fromIntegralB
  ) where

import Prelude hiding 
  ( quotRem, divMod
  , quot, rem
  , div, mod
  , properFraction
  , fromInteger, toInteger )
import qualified Prelude as P

import Control.Arrow (first)

import Data.Boolean

{--------------------------------------------------------------------
    Misc
--------------------------------------------------------------------}

infixr 9 .:

-- Double composition. (Aka "result.result". See semantic editor combinators.)
(.:) :: (c -> c') -> (a -> b -> c) -> (a -> b -> c')
(.:) = (.).(.)

(##) :: (a -> b -> c) -> (a -> b -> d) -> a -> b -> (c,d)
(f ## g) x y = (f x y, g x y)

-- -----------------------------------------------------------------------
-- Generalized Number Class Hirarchy
-- -----------------------------------------------------------------------

-- | An extension of 'Num' that supplies the integer type of a 
--   given number type and a way to create that number from the 
--   integer.
class Num a => NumB a where
  -- | The accociated integer type of the number.
  type IntegerOf a
  -- | Construct the number from the associated integer.
  fromIntegerB :: IntegerOf a -> a

-- | A deep embedded version of 'Integral'.
--   Integral numbers, supporting integer division.
--   
--   Minimal complete definition is either 'quotRem' and 'divMod'
--   or the other four functions. Besides that 'toIntegerB' always
--   has to be implemented.
class (NumB a, OrdB a) => IntegralB a where
  -- | Integer division truncated towards zero.
  quot :: a -> a -> a
  quot = fst .: quotRem
  -- | Integer reminder, satisfying:
  --   @(x `quot` y) * y + (x `rem` y) == x@
  rem :: a -> a -> a
  rem = snd .: quotRem
  -- | Integer division truncated toward negative infinity.
  div :: a -> a -> a
  div = fst .: divMod
  -- | Integer modulus, satisfying:
  --   @(x `div` y) * y + (x `mod` y) == x@
  mod :: a -> a -> a
  mod = snd .: divMod
  -- | Simultaneous 'quot' and 'rem'.
  quotRem :: a -> a -> (a,a)
  quotRem = quot ## rem
  -- | Simultaneous 'div' and 'mod'.
  divMod :: a -> a -> (a,a)
  divMod  = div ## mod
  -- | Create a integer from this integral.
  toIntegerB :: a -> IntegerOf a

-- | Deep embedded version of 'RealFloat'.
--   Extracting components of fractions.
--   
--   Minimal complete definition: 'properFraction', 
--   'round', 'floor' and 'ceiling'.
class (NumB a, OrdB a, Fractional a) => RealFracB a where
  -- | The function 'properFraction' takes a real fractional number @x@
  -- and returns a pair @(n,f)@ such that @x = n+f@, and:
  -- 
  -- * @n@ is an integral number with the same sign as @x@; and
  -- 
  -- * @f@ is a fraction with the same type and sign as @x@,
  --   and with absolute value less than @1@.
  --   
  -- The default definitions of the 'ceiling', 'floor', 'truncate'
  -- and 'round' functions are in terms of 'properFraction'.
  properFraction :: (IntegerOf a ~ IntegerOf b, IntegralB b) => a -> (b, a)
  -- | @'truncate' x@ returns the integer nearest @x@ between zero and @x@
  truncate :: (IntegerOf a ~ IntegerOf b, IntegralB b) => a -> b
  truncate = fst . properFraction
  -- | @'round' x@ returns the nearest integer to @x@;
  --   the even integer if @x@ is equidistant between two integers
  round :: (IntegerOf a ~ IntegerOf b, IntegralB b) => a -> b
  -- | @'ceiling' x@ returns the least integer not less than @x@
  ceiling :: (IntegerOf a ~ IntegerOf b, IntegralB b) => a -> b
  -- | @'floor' x@ returns the greatest integer not greater than @x@.
  floor :: (IntegerOf a ~ IntegerOf b, IntegralB b) => a -> b

-- | Deep embedded version of 'RealFloat'.
--   Efficient, machine-independent access to the components of a
--   floating-point number.
--   
--   A complete definition has to define all functions.
class (Boolean (BooleanOf a), RealFracB a, Floating a) => RealFloatB a where
  -- | 'true' if the argument is an IEEE \"not-a-number\" (NaN) value.
  isNaN :: a -> BooleanOf a
  -- | 'true' if the argument is an IEEE infinity or negative infinity.
  isInfinite :: a -> BooleanOf a
  -- | 'true' if the argument is an IEEE negative zero.
  isNegativeZero :: a -> BooleanOf a
  -- | 'true' if the argument is an IEEE floating point number.
  isIEEE :: a -> BooleanOf a
  -- | a version of arctangent taking two real floating-point arguments.
  --   For real floating @x@ and @y@, @'atan2' y x@ computes the angle
  --   (from the positive x-axis) of the vector from the origin to the
  --   point @(x,y)@.  @'atan2' y x@ returns a value in the range [@-pi@,
  --   @pi@].  It follows the Common Lisp semantics for the origin when
  --   signed zeroes are supported.  @'atan2' y 1@, with @y@ in a type
  --   that is 'RealFloatB', should return the same value as @'atan' y@.
  atan2 :: a -> a -> a

-- -----------------------------------------------------------------------
-- Generalized Number Utility Functions
-- -----------------------------------------------------------------------

-- | Variant of 'even' for generalized booleans.
evenB :: (IfB a, EqB a, IntegralB a) => a -> BooleanOf a
evenB n = n `rem` 2 ==* 0

-- | Variant of 'odd' for generalized booleans.
oddB :: (IfB a, EqB a, IntegralB a) => a -> BooleanOf a
oddB = notB . evenB

-- | Variant of 'fromIntegral' for generalized booleans.
fromIntegralB :: (IntegerOf a ~ IntegerOf b, IntegralB a, NumB b) => a -> b
fromIntegralB = fromIntegerB . toIntegerB

-- -----------------------------------------------------------------------
-- Default Class Instances for Basic Types
-- -----------------------------------------------------------------------

-- | Only for internal use.
fromInteger' :: (Integer ~ IntegerOf b, NumB b) => Integer -> b
fromInteger' = fromIntegralB

#define DefaultNumBInstance(Ty) \
instance NumB (Ty) where {\
  type IntegerOf (Ty) = Integer ;\
  fromIntegerB = P.fromInteger }

#define DefaultIntegralBInstance(Ty) \
instance IntegralB (Ty) where {\
  quotRem = P.quotRem ;\
  divMod = P.divMod ;\
  toIntegerB = P.toInteger }

#define DefaultRealFracFloatBInstance(Ty) \
instance RealFracB (Ty) where {\
  properFraction = first fromInteger' . P.properFraction ;\
  round          = fromInteger' . P.round ;\
  floor          = fromInteger' . P.floor ;\
  ceiling        = fromInteger' . P.ceiling };\
instance RealFloatB (Ty) where {\
  isNaN          = P.isNaN ;\
  isInfinite     = P.isInfinite ;\
  isNegativeZero = P.isNegativeZero ;\
  isIEEE         = P.isIEEE ;\
  atan2          = P.atan2 }

DefaultNumBInstance(Int)
DefaultNumBInstance(Integer)
DefaultNumBInstance(Float)
DefaultNumBInstance(Double)

DefaultIntegralBInstance(Int)
DefaultIntegralBInstance(Integer)

DefaultRealFracFloatBInstance(Float)
DefaultRealFracFloatBInstance(Double)
