{-# LANGUAGE RebindableSyntax #-}
{- |
Module      :  Number.Ratio
Copyright   :  (c) Henning Thielemann 2011-2012
               (c) Dylan Thurston 2006

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  portable (?)

Ratios of mathematical objects.
-}

module Number.Ratio
        (
          T((:%), numerator, denominator), (%),
          Rational,
          fromValue,
          recip,

          scale,
          split,
          showsPrecAuto,

          toRational98,
        )  where

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Units                as Unit
import qualified Algebra.Absolute             as Absolute
import qualified Algebra.Ring                 as Ring
import qualified Algebra.Additive             as Additive
import qualified Algebra.ZeroTestable         as ZeroTestable
import qualified Algebra.Indexable            as Indexable

import Algebra.PrincipalIdealDomain (gcd, )
import Algebra.Units (stdUnitInv, stdAssociate, )
import Algebra.IntegralDomain (div, divMod, )
import Algebra.Ring (one, (*), (^), fromInteger, )
import Algebra.Additive (zero, (+), (-), negate, )
import Algebra.ZeroTestable (isZero, )

import Control.Monad(liftM, liftM2, )

import Foreign.Storable (Storable (..), )
import qualified Foreign.Storable.Record as Store
import Control.Applicative (liftA2, )

import Test.QuickCheck (Arbitrary(arbitrary))
import System.Random (Random(..), RandomGen, )

import qualified Data.Ratio as Ratio98

import qualified Prelude as P
import NumericPrelude.Base


infixl 7 %

data  {- (PID.C a)  => -} T a = (:%) {
        numerator   :: !a,
        denominator :: !a
     } deriving (Eq)
type  Rational = T P.Integer


fromValue :: Ring.C a => a -> T a
fromValue x = x :% one

scale :: (PID.C a) => a -> T a -> T a
scale s (x:%y) =
   let {- x and y are cancelled,
          thus we can only have common divisors in s and y -}
       (n:%d) = s%y
   in  ((n*x):%d)

{- | similar to 'Algebra.RealRing.splitFraction' -}
split :: (PID.C a) => T a -> (a, T a)
split (x:%y) =
   let (q,r) = divMod x y
   in  (q, r:%y)

ratioPrec :: P.Int
ratioPrec = 7

(%) :: (PID.C a) => a -> a -> T a
x % y =
  if isZero y
    then error "NumericPrelude.% : zero denominator"
    else
      let d  = gcd x y
          y0 = div y d
          x0 = div x d
      in  (stdUnitInv y0 * x0) :% stdAssociate y0

instance (PID.C a) => Additive.C (T a) where
    zero                =  fromValue zero
--    (x:%y) + (x':%y')   =  (x*y' + x'*y) % (y*y')
    {-
    This version reduces the size of intermediate results.
    Is it also faster than the naive version?
    The final (%) includes another gcd computation,
    but it is still needed since e.g.
    5:%7 + (-5):%7 shall be simplified to 0:%1, not 0:%7 .
    -}
    (x:%y) + (x':%y')   =
       let d = gcd y y'
           y0  = div y  d
           y0' = div y' d
       in  (x*y0' + x'*y0) % (y0*y')
    negate (x:%y)       =  (-x) :% y

instance (PID.C a) => Ring.C (T a) where
    one                 =  fromValue one
    fromInteger x       =  fromValue $ fromInteger x
    (x:%y) * (x':%y')   =  (x * x') % (y * y')
    (x:%y) ^ n          =  (x ^ n) :% (y ^ n)

instance (Absolute.C a, PID.C a) => Absolute.C (T a) where
    abs (x:%y)          =  Absolute.abs x :% y
    signum (x:%_)       =  Absolute.signum x :% one

recip :: (ZeroTestable.C a, Unit.C a) => T a -> T a
recip (x:%y) =
   if isZero y
     then error "Ratio.recip: division by zero"
     else (y * stdUnitInv x) :% stdAssociate x


liftOrd :: Ring.C a => (a -> a -> b) -> (T a -> T a -> b)
liftOrd f (x:%y) (x':%y') = f (x * y') (x' * y)

instance (Ord a, PID.C a) => Ord (T a) where
    (<=)     =  liftOrd (<=)
    (<)      =  liftOrd (<)
    (>=)     =  liftOrd (>=)
    (>)      =  liftOrd (>)
    compare  =  liftOrd compare

instance (Ord a, PID.C a) => Indexable.C (T a) where
    compare  =  compare

instance (ZeroTestable.C a, PID.C a) => ZeroTestable.C (T a) where
    isZero  =  isZero . numerator

instance  (Read a, PID.C a)  => Read (T a)  where
    readsPrec p  =
       readParen (p >= ratioPrec)
                 (\r -> [(x%y,u) | (x,s)   <- readsPrec ratioPrec r,
                                   ("%",t) <- lex s,
                                   (y,u)   <- readsPrec ratioPrec t ])

instance  (Show a, PID.C a)  => Show (T a)  where
    showsPrec p (x:%y)  =  showParen (p >= ratioPrec)
                               (shows x . showString " % " . shows y)

{- |
This is an alternative show method
that is more user-friendly but also potentially more ambigious.
-}

showsPrecAuto :: (Eq a, PID.C a, Show a) =>
   P.Int -> T a -> String -> String
showsPrecAuto p (x:%y) =
   if y == 1
     then showsPrec p x
     else showParen (p > ratioPrec)
             (showsPrec (ratioPrec+1) x . showString "/" .
              showsPrec (ratioPrec+1) y)


instance (Arbitrary a, PID.C a, ZeroTestable.C a) => Arbitrary (T a) where
{-
   arbitrary = liftM2 (%) arbitrary (untilM (not . isZero) arbitrary)

This implementation leads to blocking:

*Main> Test.QuickCheck.test (\x -> x==(x::Rational))
Interrupted.
-}
   arbitrary =
      liftM2 (%) arbitrary
         (liftM (\x -> if isZero x then one else x) arbitrary)


instance (Storable a, PID.C a) => Storable (T a) where
   sizeOf    = Store.sizeOf store
   alignment = Store.alignment store
   peek      = Store.peek store
   poke      = Store.poke store

store ::
   (Storable a, PID.C a) =>
   Store.Dictionary (T a)
store =
   Store.run $
   liftA2 (%)
      (Store.element numerator)
      (Store.element denominator)

{-
This instance may not be appropriate for mathematical objects other than numbers.
If we encounter such a type of object
we should define an intermediate class
which provides the necessary functions.
I should remark that methods of Random like 'randomR'
cannot sensibly be defined for ratios of polynomials.
-}
instance (Random a, PID.C a, ZeroTestable.C a) => Random (T a) where
   random g0 =
      let (numer, g1) = random g0
          (denom, g2) = random g1
      in  (numer % if isZero denom then one else denom, g2)
   randomR (lower,upper) g0 =
      let (k, g1) = randomR01 g0
      in  (lower + k*(upper-lower), g1)


randomR01 ::
   (Random a, PID.C a, RandomGen g) =>
   g -> (T a, g)
randomR01 g0 =
   let (denom0, g1) = random g0
       denom = if isZero denom0 then one else denom0
       (numer, g2) = randomR (zero,denom) g1
   in  (numer % denom, g2)


-- * Legacy Instances


-- | Necessary when mixing NumericPrelude.Numeric Rationals with Prelude98 Rationals

toRational98 :: (P.Integral a) => T a -> Ratio98.Ratio a
toRational98 x = numerator x Ratio98.% denominator x

fromRational98 :: (P.Integral a) => Ratio98.Ratio a -> T a
fromRational98 x = Ratio98.numerator x :% Ratio98.denominator x


{-# INLINE lift1 #-}
lift1 ::
   (P.Integral a, P.Integral b) =>
   (Ratio98.Ratio a -> Ratio98.Ratio b) -> T a -> T b
lift1 f a = fromRational98 (f (toRational98 a))

{-# INLINE lift2 #-}
lift2 ::
   (P.Integral a, P.Integral b, P.Integral c) =>
   (Ratio98.Ratio a -> Ratio98.Ratio b -> Ratio98.Ratio c) -> T a -> T b -> T c
lift2 f a b = fromRational98 (f (toRational98 a) (toRational98 b))


instance (P.Integral a) => P.Num (T a) where
   fromInteger n = P.fromInteger n :% P.fromInteger 1
   negate = lift1 P.negate
   (+)    = lift2 (P.+)
   (*)    = lift2 (P.*)
   abs    = lift1 P.abs
   signum = lift1 P.signum

instance (P.Integral a) => P.Fractional (T a) where
   fromRational x =
      P.fromInteger (Ratio98.numerator x) :%
      P.fromInteger (Ratio98.denominator x)
   (/) = lift2 (P./)
   recip = lift1 P.recip

{- causes an import cycle
instance (P.Integral a) => P.Num (T a) where
   fromInteger n = P.fromInteger n % 1
   negate = W98.unliftF1 P.negate
   (+)    = W98.unliftF2 (+)
   (*)    = W98.unliftF2 (*)
   abs    = W98.unliftF1 abs
   signum = W98.unliftF1 P.signum

instance (P.Integral a) => P.Fractional (T a) where
--   fromRational = Field.fromRational
   fromRational x =
      fromInteger (Ratio98.numerator x) :%
      fromInteger (Ratio98.denominator x)
   recip = recip
-}
