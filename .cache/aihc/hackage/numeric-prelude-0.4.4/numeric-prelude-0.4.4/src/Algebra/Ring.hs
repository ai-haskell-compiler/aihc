{-# LANGUAGE RebindableSyntax #-}
module Algebra.Ring (
    {- * Class -}
    C,

    (*),
    one,
    fromInteger,
    (^), sqr,

    {- * Complex functions -}
    product, product1, scalarProduct,

    {- * Properties -}
    propAssociative,
    propLeftDistributive,
    propRightDistributive,
    propLeftIdentity,
    propRightIdentity,
    propPowerCascade,
    propPowerProduct,
    propPowerDistributive,
    propCommutative,
  ) where

import qualified Algebra.Additive as Additive
import qualified Algebra.Laws as Laws

import Algebra.Additive(zero, (+), negate, sum)

import Data.Function.HT (powerAssociative, )
import NumericPrelude.List (zipWithChecked, )

import Test.QuickCheck ((==>), Property)

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import NumericPrelude.Base
import Prelude (Integer, Float, Double, )
import qualified Data.Complex as Complex98
import qualified Data.Ratio as Ratio98
import qualified Prelude as P


infixl 7 *
infixr 8 ^


{- |
Ring encapsulates the mathematical structure
of a (not necessarily commutative) ring, with the laws

@
  a * (b * c) === (a * b) * c
      one * a === a
      a * one === a
  a * (b + c) === a * b + a * c
@

Typical examples include integers, polynomials, matrices, and quaternions.

Minimal definition: '*', ('one' or 'fromInteger')
-}

class (Additive.C a) => C a where
    {-# MINIMAL (*), (one | fromInteger) #-}
    (*)         :: a -> a -> a
    one         :: a
    fromInteger :: Integer -> a
    {- |
    The exponent has fixed type 'Integer' in order
    to avoid an arbitrarily limitted range of exponents,
    but to reduce the need for the compiler to guess the type (default type).
    In practice the exponent is most oftenly fixed, and is most oftenly @2@.
    Fixed exponents can be optimized away and
    thus the expensive computation of 'Integer's doesn't matter.
    The previous solution used a 'Algebra.ToInteger.C' constrained type
    and the exponent was converted to Integer before computation.
    So the current solution is not less efficient.

    A variant of '^' with more flexibility is provided by 'Algebra.Core.ringPower'.
    -}
    (^)         :: a -> Integer -> a

    {-# INLINE fromInteger #-}
    fromInteger n = if n < 0
                      then powerAssociative (+) zero (negate one) (negate n)
                      else powerAssociative (+) zero one n
    {-# INLINE (^) #-}
    a ^ n = if n >= zero
              then powerAssociative (*) one a n
              else error "(^): Illegal negative exponent"
    {-# INLINE one #-}
    one = fromInteger 1


sqr :: C a => a -> a
sqr x = x*x

product :: (C a) => [a] -> a
product = foldl (*) one

product1 :: (C a) => [a] -> a
product1 = foldl1 (*)


scalarProduct :: C a => [a] -> [a] -> a
scalarProduct as bs = sum (zipWithChecked (*) as bs)


{- * Instances for atomic types -}

instance C Integer where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Float   where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Double  where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)


instance C Int     where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Int8    where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Int16   where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Int32   where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Int64   where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)


instance C Word    where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Word8   where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Word16  where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Word32  where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)

instance C Word64  where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one         = P.fromInteger 1
   fromInteger = P.fromInteger
   (*)         = (P.*)





propAssociative       :: (Eq a, C a) => a -> a -> a -> Bool
propLeftDistributive  :: (Eq a, C a) => a -> a -> a -> Bool
propRightDistributive :: (Eq a, C a) => a -> a -> a -> Bool
propLeftIdentity      :: (Eq a, C a) => a -> Bool
propRightIdentity     :: (Eq a, C a) => a -> Bool

propAssociative       =  Laws.associative (*)
propLeftDistributive  =  Laws.leftDistributive  (*) (+)
propRightDistributive =  Laws.rightDistributive (*) (+)
propLeftIdentity      =  Laws.leftIdentity  (*) one
propRightIdentity     =  Laws.rightIdentity (*) one

propPowerCascade      :: (Eq a, C a) => a -> Integer -> Integer -> Property
propPowerProduct      :: (Eq a, C a) => a -> Integer -> Integer -> Property
propPowerDistributive :: (Eq a, C a) => Integer -> a -> a -> Property

propPowerCascade      x i j  =  i>=0 && j>=0 ==> Laws.rightCascade (*) (^) x i j
propPowerProduct      x i j  =  i>=0 && j>=0 ==> Laws.homomorphism (x^) (+) (*) i j
propPowerDistributive i x y  =  i>=0 ==> Laws.leftDistributive (^) (*) i x y

{- | Commutativity need not be satisfied by all instances of 'Algebra.Ring.C'. -}
propCommutative :: (Eq a, C a) => a -> a -> Bool

propCommutative  =  Laws.commutative (*)


-- legacy

instance (P.Integral a) => C (Ratio98.Ratio a) where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one                 =  P.fromInteger 1
   fromInteger         =  P.fromInteger
   (*)                 =  (P.*)

instance (P.RealFloat a) => C (Complex98.Complex a) where
   {-# INLINE one #-}
   {-# INLINE fromInteger #-}
   {-# INLINE (*) #-}
   one                 =  P.fromInteger 1
   fromInteger         =  P.fromInteger
   (*)                 =  (P.*)
