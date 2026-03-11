{-# LANGUAGE RebindableSyntax #-}
module Algebra.Absolute (
   C(abs, signum),
   absOrd, signumOrd,
   ) where

import qualified Algebra.Ring         as Ring
import qualified Algebra.Additive     as Additive

import Algebra.Ring (one, )
import Algebra.Additive (zero, negate,)

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import NumericPrelude.Base
import qualified Prelude as P
import Prelude (Integer, Float, Double, )


{- |
This is the type class of a ring with a notion of an absolute value,
satisfying the laws

>                        a * b === b * a
>   a /= 0  =>  abs (signum a) === 1
>             abs a * signum a === a

Minimal definition: 'abs', 'signum'.

If the type is in the 'Ord' class
we expect 'abs' = 'absOrd' and 'signum' = 'signumOrd'
and we expect the following laws to hold:

>      a + (max b c) === max (a+b) (a+c)
>   negate (max b c) === min (negate b) (negate c)
>      a * (max b c) === max (a*b) (a*c) where a >= 0
>           absOrd a === max a (-a)

If the type is @ZeroTestable@, then it should hold

>  isZero a  ===  signum a == signum (negate a)

We do not require 'Ord' as superclass
since we also want to have "Number.Complex" as instance.
We also do not require @ZeroTestable@ as superclass,
because we like to have expressions of foreign languages
to be instances (cf. embedded domain specific language approach, EDSL),
as well as function types.

'abs' for complex numbers alone may have an inappropriate type,
because it does not reflect that the absolute value is a real number.
You might prefer 'Number.Complex.magnitude'.
This type class is intended for unifying algorithms
that work for both real and complex numbers.
Note the similarity to "Algebra.Units":
'abs' plays the role of @stdAssociate@
and 'signum' plays the role of @stdUnit@.

Actually, since 'abs' can be defined using 'max' and 'negate'
we could relax the superclasses to @Additive@ and 'Ord'
if his class would only contain 'signum'.
-}
class (Ring.C a) => C a where
    abs    :: a -> a
    signum :: a -> a


absOrd :: (Additive.C a, Ord a) => a -> a
absOrd x = max x (negate x)

signumOrd :: (Ring.C a, Ord a) => a -> a
signumOrd x =
   case compare x zero of
      GT ->        one
      EQ ->        zero
      LT -> negate one


instance C Integer where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Float   where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Double  where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum


instance C Int     where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Int8    where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Int16   where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Int32   where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Int64   where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum


instance C Word    where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Word8   where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Word16  where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Word32  where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

instance C Word64  where
   {-# INLINE abs #-}
   {-# INLINE signum #-}
   abs = P.abs
   signum = P.signum

