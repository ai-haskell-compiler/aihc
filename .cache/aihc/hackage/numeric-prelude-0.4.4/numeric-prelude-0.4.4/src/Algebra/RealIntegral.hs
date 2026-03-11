{-# LANGUAGE RebindableSyntax #-}
{- |
Generally before using 'quot' and 'rem', think twice.
In most cases 'divMod' and friends are the right choice,
because they fulfill more of the wanted properties.
On some systems 'quot' and 'rem' are more efficient
and if you only use positive numbers, you may be happy with them.
But we cannot warrant the efficiency advantage.

See also:
Daan Leijen: Division and Modulus for Computer Scientists
<http://www.cs.uu.nl/%7Edaan/download/papers/divmodnote-letter.pdf>,
<http://www.haskell.org/pipermail/haskell-cafe/2007-August/030394.html>
-}
module Algebra.RealIntegral (
   C(quot, rem, quotRem),
   ) where

import qualified Algebra.ZeroTestable   as ZeroTestable
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Absolute       as Absolute

import Algebra.Absolute (signum, )
import Algebra.IntegralDomain (divMod, )
import Algebra.Ring (one, ) -- fromInteger
import Algebra.Additive (zero, (+), (-), )

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import NumericPrelude.Base
import qualified Prelude as P
import Prelude (Integer, )


infixl 7 `quot`, `rem`

{- |
Remember that 'divMod' does not specify exactly what @a `quot` b@ should be,
mainly because there is no sensible way to define it in general.
For an instance of @Algebra.RealIntegral.C a@,
it is expected that @a `quot` b@ will round towards 0 and
@a `Prelude.div` b@ will round towards minus infinity.

Minimal definition: nothing required
-}

class (Absolute.C a, ZeroTestable.C a, Ord a, Integral.C a) => C a where
    quot, rem        :: a -> a -> a
    quotRem          :: a -> a -> (a,a)

    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE quotRem #-}
    quot a b = fst (quotRem a b)
    rem a b  = snd (quotRem a b)
    quotRem a b = let (d,m) = divMod a b in
                   if (signum d < zero) then
                         (d+one,m-b) else (d,m)


instance C Integer where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Int     where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Int8    where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Int16   where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Int32   where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Int64   where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem


instance C Word    where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Word8   where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Word16  where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Word32  where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

instance C Word64  where
   {-# INLINE quot #-}
   {-# INLINE rem #-}
   {-# INLINE quotRem #-}
   quot = P.quot
   rem = P.rem
   quotRem = P.quotRem

