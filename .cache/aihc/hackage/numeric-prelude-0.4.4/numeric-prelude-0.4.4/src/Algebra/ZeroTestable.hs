{-# LANGUAGE RebindableSyntax #-}
module Algebra.ZeroTestable where

import qualified Algebra.Additive as Additive

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import Prelude (Integer, Float, Double, )
import NumericPrelude.Base

{- |
Maybe the naming should be according to Algebra.Unit:
Algebra.Zero as module name, and @query@ as method name.
-}
class C a where
   isZero :: a -> Bool

{- |
Checks if a number is the zero element.
This test is not possible for all 'Additive.C' types,
since e.g. a function type does not belong to Eq.
isZero is possible for some types where (==zero) fails
because there is no unique zero.
Examples are
vector (the length of the zero vector is unknown),
physical values (the unit of a zero quantity is unknown),
residue class (the modulus is unknown).
-}
defltIsZero :: (Eq a, Additive.C a) => a -> Bool
defltIsZero = (Additive.zero==)


{-* Instances for atomic types -}

instance C Integer where isZero = defltIsZero
instance C Float   where isZero = defltIsZero
instance C Double  where isZero = defltIsZero

instance C Int     where isZero = defltIsZero
instance C Int8    where isZero = defltIsZero
instance C Int16   where isZero = defltIsZero
instance C Int32   where isZero = defltIsZero
instance C Int64   where isZero = defltIsZero

instance C Word    where isZero = defltIsZero
instance C Word8   where isZero = defltIsZero
instance C Word16  where isZero = defltIsZero
instance C Word32  where isZero = defltIsZero
instance C Word64  where isZero = defltIsZero



{-* Instances for composed types -}

instance (C v0, C v1) => C (v0, v1) where
    isZero (x0,x1) = isZero x0 && isZero x1

instance (C v0, C v1, C v2) => C (v0, v1, v2) where
    isZero (x0,x1,x2) = isZero x0 && isZero x1 && isZero x2


instance (C v) => C [v] where
    isZero = all isZero
