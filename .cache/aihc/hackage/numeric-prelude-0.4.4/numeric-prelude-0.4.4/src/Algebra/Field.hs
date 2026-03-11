{-# LANGUAGE RebindableSyntax #-}
module Algebra.Field (
    {- * Class -}
    C,

    (/),
    recip,
    fromRational',
    fromRational,
    (^-),

    {- * Properties -}
    propDivision,
    propReciprocal,
  ) where

import Number.Ratio (T((:%)), Rational, (%), numerator, denominator, )
import qualified Number.Ratio as Ratio
import qualified Data.Complex as Complex98
import qualified Data.Ratio as Ratio98
import qualified Algebra.PrincipalIdealDomain as PID

import qualified Algebra.Ring         as Ring
import qualified Algebra.ZeroTestable as ZeroTestable

import Algebra.Ring ((*), (^), one, fromInteger)
import Algebra.Additive (zero, negate)
import Algebra.ZeroTestable (isZero)

import NumericPrelude.Base
import Prelude (Integer, Float, Double)
import qualified Prelude as P
import Test.QuickCheck ((==>), Property)


infixr 8 ^-
infixl 7 /


{- |
Field again corresponds to a commutative ring.
Division is partially defined and satisfies

>    not (isZero b)  ==>  (a * b) / b === a
>    not (isZero a)  ==>  a * recip a === one

when it is defined. 
To safely call division,
the program must take type-specific action;
e.g., the following is appropriate in many cases:

> safeRecip :: (Integral a, Eq a, Field.C a) => a -> Maybe a
> safeRecip x =
>     let (q,r) = one `divMod` x
>     in  toMaybe (isZero r) q

Typical examples include rationals, the real numbers,
and rational functions (ratios of polynomial functions).
An instance should be typically declared
only if most elements are invertible.

Actually, we have also used this type class for non-fields
containing lots of units,
e.g. residue classes with respect to non-primes and power series.
So the restriction @not (isZero a)@ must be better @isUnit a@.

Minimal definition: 'recip' or ('/')
-}

class (Ring.C a) => C a where
    {-# MINIMAL recip | (/) #-}
    (/)           :: a -> a -> a
    recip         :: a -> a
    fromRational' :: Rational -> a
    (^-)          :: a -> Integer -> a

    {-# INLINE recip #-}
    recip a = one / a
    {-# INLINE (/) #-}
    a / b = a * recip b
    {-# INLINE fromRational' #-}
    fromRational' r = fromInteger (numerator r) / fromInteger (denominator r)
    {-# INLINE (^-) #-}
    a ^- n = if n < zero
               then recip (a^(-n))
               else a^n
 -- a ^ n | n < 0 = reduceRepeated (^) one (recip a) (negate (toInteger n))
 --       | True  = reduceRepeated (^) one a (toInteger n)



-- | Needed to work around shortcomings in GHC.

{-# INLINE fromRational #-}
fromRational :: (C a) => P.Rational -> a
fromRational x = fromRational' (Ratio98.numerator x :% Ratio98.denominator x)


{- * Instances for atomic types -}

{-
fromRational must be implemented explicitly for Float and Double!
It may be that numerator or denominator cannot be represented as Float
due to size constraints, but the fraction can.
-}

instance C Float where
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    (/)    = (P./)
    recip  = (P.recip)
    -- using Ratio98.:% would be more efficient but it is not exported.
    fromRational' x =
       P.fromRational (numerator x Ratio98.% denominator x)

instance C Double where
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    (/)    = (P./)
    recip  = (P.recip)
    fromRational' x =
       P.fromRational (numerator x Ratio98.% denominator x)

instance (PID.C a) => C (Ratio.T a) where
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    {-# INLINE fromRational' #-}
--    (/)                  =  Ratio.liftOrd (%)
    x / y                =  x * recip y
{-
This is efficient and almost correct in the sense,
that all admissible cases yield a correct result.
However it will hide division by zero and thus may hide bugs.
Unfortunately 'x' might not be a standard associate,
thus (y:%x) may deviate from the canonical representation.

    recip (x:%y)         =  (y:%x)
-}
    recip = Ratio.recip
    fromRational' (x:%y) =  fromInteger x % fromInteger y


-- | the restriction on the divisor should be @isUnit a@ instead of @not (isZero a)@
propDivision   :: (Eq a, ZeroTestable.C a, C a) => a -> a -> Property
propReciprocal :: (Eq a, ZeroTestable.C a, C a) => a -> Property

propDivision   a b   =   not (isZero b)  ==>  (a * b) / b == a
propReciprocal a     =   not (isZero a)  ==>  a * recip a == one



-- legacy

instance (P.Integral a) => C (Ratio98.Ratio a) where
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    (/)    = (P./)
    recip  = (P.recip)

instance (P.RealFloat a) => C (Complex98.Complex a) where
    {-# INLINE (/) #-}
    {-# INLINE recip #-}
    (/)    = (P./)
    recip  = (P.recip)
