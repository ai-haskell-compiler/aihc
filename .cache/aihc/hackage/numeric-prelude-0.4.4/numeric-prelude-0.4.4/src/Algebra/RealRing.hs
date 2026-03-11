{-# LANGUAGE RebindableSyntax #-}
module Algebra.RealRing where

import qualified Algebra.RealRing98 as RealRing98

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.ToRational     as ToRational
import qualified Algebra.ToInteger      as ToInteger
import qualified Algebra.Absolute       as Absolute

import qualified Algebra.OrderDecision as OrdDec
import Algebra.OrderDecision ((<?), (>=?), )

import Algebra.Field          (fromRational, )
import Algebra.RealIntegral   (quotRem, )
import Algebra.IntegralDomain (divMod, even, )
import Algebra.Ring           ((*), fromInteger, one, )
import Algebra.Additive       ((+), (-), negate, zero, )
import Algebra.ZeroTestable   (isZero, )
import Algebra.ToInteger      (fromIntegral, )

import qualified Number.Ratio as Ratio
import Number.Ratio (T((:%)), Rational)

import Data.Int  (Int,  Int8,  Int16,  Int32,  Int64,  )
import Data.Word (Word, Word8, Word16, Word32, Word64, )

import qualified GHC.Float as GHC
import Data.List as List
import Data.Tuple.HT (mapFst, mapPair, )
import Prelude (Integer, Float, Double, )
import qualified Prelude as P
import NumericPrelude.Base


{- $setup
>>> import qualified Algebra.RealRing as RealRing
>>> import Data.Tuple.HT (mapFst)
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base
>>> import Prelude ()
>>>
>>> infix 4 =~=
>>>
>>> (=~=) :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
>>> (f =~= g) x = f x == g x
-}


{- |
Minimal complete definition:
     'splitFraction' or 'floor'

There are probably more laws, but some laws are

> splitFraction x === (fromInteger (floor x), fraction x)
> fromInteger (floor x) + fraction x === x
> floor x       <= x       x <  floor x + 1
> ceiling x - 1 <  x       x <= ceiling x
> 0 <= fraction x          fraction x < 1

>               - ceiling x === floor (-x)
>                truncate x === signum x * floor (abs x)
>    ceiling (toRational x) === ceiling x :: Integer
>   truncate (toRational x) === truncate x :: Integer
>      floor (toRational x) === floor x :: Integer

The new function 'fraction' doesn't return the integer part of the number.
This also removes a type ambiguity if the integer part is not needed.

Many people will associate rounding with fractional numbers,
and thus they are surprised about the superclass being @Ring@ not @Field@.
The reason is that all of these methods can be defined
exclusively with functions from @Ord@ and @Ring@.
The implementations of 'genericFloor' and other functions demonstrate that.
They implement power-of-two-algorithms
like the one for finding the number of digits of an 'Integer'
in FixedPoint-fractions module.
They are even reasonably efficient.

I am still uncertain whether it was a good idea
to add instances for @Integer@ and friends,
since calling @floor@ or @fraction@ on an integer may well indicate a bug.
The rounding functions are just the identity function
and 'fraction' is constant zero.
However, I decided to associate our class with @Ring@ rather than @Field@,
after I found myself using repeated subtraction and testing
rather than just calling @fraction@,
just in order to get the constraint @(Ring a, Ord a)@
that was more general than @(RealField a)@.

For the results of the rounding functions
we have chosen the constraint @Ring@ instead of @ToInteger@,
since this is more flexible to use,
but it still signals to the user that only integral numbers can be returned.
This is so, because the plain @Ring@ class only provides
@zero@, @one@ and operations that allow to reach all natural numbers but not more.


As an aside, let me note the similarities
between @splitFraction x@ and @divMod x 1@ (if that were defined).
In particular, it might make sense to unify the rounding modes somehow.

The new methods 'fraction' and 'splitFraction'
differ from 'Prelude.properFraction' semantics.
They always round to 'floor'.
This means that the fraction is always non-negative and
is always smaller than 1.
This is more useful in practice and
can be generalised to more than real numbers.
Since every 'Number.Ratio.T' denominator type
supports 'Algebra.IntegralDomain.divMod',
every 'Number.Ratio.T' can provide 'fraction' and 'splitFraction',
e.g. fractions of polynomials.
However the @Ring@ constraint for the ''integral'' part of 'splitFraction'
is too weak in order to generate polynomials.
After all, I am uncertain whether this would be useful or not.

Can there be a separate class for
'fraction', 'splitFraction', 'floor' and 'ceiling'
since they do not need reals and their ordering?

We might also add a round method,
that rounds 0.5 always up or always down.
This is much more efficient in inner loops
and is acceptable or even preferable for many applications.
-}

class (Absolute.C a, Ord a) => C a where
    {-# MINIMAL splitFraction | floor #-}
    {- |
    prop> \x -> (x::Rational) == (uncurry (+) $ mapFst fromInteger $ splitFraction x)
    prop> \x -> uncurry (==) $ mapFst (((x::Double)-) . fromInteger) $ splitFraction x
    prop> \x -> uncurry (==) $ mapFst (((x::Rational)-) . fromInteger) $ splitFraction x
    prop> \x -> splitFraction x == (floor (x::Double) :: Integer, fraction x)
    prop> \x -> splitFraction x == (floor (x::Rational) :: Integer, fraction x)
    -}
    splitFraction    :: (Ring.C b) => a -> (b,a)
    {- |
    prop> \x -> let y = fraction (x::Double) in 0<=y && y<1
    prop> \x -> let y = fraction (x::Rational) in 0<=y && y<1
    -}
    fraction :: a -> a
    {- |
    prop> \x -> ceiling (-x) == negate (floor (x::Double) :: Integer)
    prop> \x -> ceiling (-x) == negate (floor (x::Rational) :: Integer)
    -}
    ceiling, floor   :: (Ring.C b) => a -> b
    truncate         :: (Ring.C b) => a -> b
    round            :: (ToInteger.C b) => a -> b


    splitFraction x   =  (floor x, fraction x)

    fraction x   =  x - fromInteger (floor x)

    floor x      =  fromInteger (fst (splitFraction x))

    ceiling x    =  - floor (-x)

--    truncate x   =  signum x * floor (abs x)
    truncate x =
       if x>=0
         then floor x
         else ceiling x

    {-
    The ToInteger constraint can be lifted to Ring
    if use Integer temporarily.
    I expect this would not be efficient in many cases.
    -}
    round x =
       let (n,r) = splitFraction x
       in  case compare (2*r) one of
              LT -> n
              EQ -> if even n then n else n+1
              GT -> n+1


{- |
This function rounds to the closest integer.
For @fraction x == 0.5@ it rounds away from zero.
This function is not the result of an ingenious mathematical insight,
but is simply a kind of rounding that is the fastest
on IEEE floating point architectures.
-}
{-# NOINLINE [2] roundSimple #-}
roundSimple :: (C a, Ring.C b) => a -> b
roundSimple x =
   let (n,r) = splitFraction x
   in  case compare (2*r) one of
          LT -> n
          EQ -> if x<0 then n else n+1
          GT -> n+1


instance (ToInteger.C a, PID.C a) => C (Ratio.T a) where
    splitFraction (x:%y) = (fromIntegral q, r:%y)
                               where (q,r) = divMod x y

instance C Integer where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromInteger x, zero)
    fraction      _ = zero
    floor         x = fromInteger x
    ceiling       x = fromInteger x
    round         x = fromInteger x
    truncate      x = fromInteger x

instance C Int where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Int8 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Int16 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Int32 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Int64 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Word8 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Word16 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Word32 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Word64 where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction x = (fromIntegral x, zero)
    fraction      _ = zero
    floor         x = fromIntegral x
    ceiling       x = fromIntegral x
    round         x = fromIntegral x
    truncate      x = fromIntegral x

instance C Float where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction = fastSplitFraction GHC.float2Int GHC.int2Float
    fraction      = RealRing98.fastFraction (GHC.int2Float . GHC.float2Int)
    floor         = fromInteger . P.floor
    ceiling       = fromInteger . P.ceiling
    round         = fromInteger . P.round
    truncate      = fromInteger . P.truncate

instance C Double where
    {-# INLINE splitFraction #-}
    {-# INLINE fraction #-}
    {-# INLINE floor #-}
    {-# INLINE ceiling #-}
    {-# INLINE round #-}
    {-# INLINE truncate #-}
    splitFraction = fastSplitFraction GHC.double2Int GHC.int2Double
    fraction      = RealRing98.fastFraction (GHC.int2Double . GHC.double2Int)
    floor         = fromInteger . P.floor
    ceiling       = fromInteger . P.ceiling
    round         = fromInteger . P.round
    truncate      = fromInteger . P.truncate


{-# INLINE fastSplitFraction #-}
fastSplitFraction :: (P.RealFrac a, Absolute.C a, Ring.C b) =>
   (a -> Int) -> (Int -> a) -> a -> (b,a)
fastSplitFraction trunc toFloat x =
   fixSplitFraction $
   if toFloat minBound <= x && x <= toFloat maxBound
     then case trunc x of n -> (fromIntegral n, x - toFloat n)
     else case P.properFraction x of (n,f) -> (fromInteger n, f)

{-# INLINE fixSplitFraction #-}
fixSplitFraction :: (Ring.C a, Ring.C b, Ord a) => (b,a) -> (b,a)
fixSplitFraction (n,f) =
   --  if x>=0 || f==0
   if f>=0
     then (n,   f)
     else (n-1, f+1)

{-# INLINE fixFraction #-}
fixFraction :: (Ring.C a, Ord a) => a -> a
fixFraction y =
   if y>=0 then y else y+1

{-
mapM_ (\n -> let x = fromInteger n / 10 in print (x, floorInt GHC.double2Int GHC.int2Double x)) [-20,-19..20]
-}

{-# INLINE splitFractionInt #-}
splitFractionInt :: (Ring.C a, Ord a) => (a -> Int) -> (Int -> a) -> a -> (Int, a)
splitFractionInt trunc toFloat x =
   let n = trunc x
   in  fixSplitFraction (n, x - toFloat n)

{-# INLINE floorInt #-}
floorInt :: (Ring.C a, Ord a) => (a -> Int) -> (Int -> a) -> a -> Int
floorInt trunc toFloat x =
   let n = trunc x
   in  if x >= toFloat n
         then n
         else pred n

{-# INLINE ceilingInt #-}
ceilingInt :: (Ring.C a, Ord a) => (a -> Int) -> (Int -> a) -> a -> Int
ceilingInt trunc toFloat x =
   let n = trunc x
   in  if x <= toFloat n
         then n
         else succ n

{-# INLINE roundInt #-}
roundInt :: (Field.C a, Ord a) => (a -> Int) -> (Int -> a) -> a -> Int
roundInt trunc toFloat x =
   let half = 0.5 -- P.fromRational
       halfUp = x+half
       n = floorInt trunc toFloat halfUp
   in  if toFloat n == halfUp  &&  P.odd n
         then pred n
         else n

{-# INLINE roundSimpleInt #-}
roundSimpleInt ::
   (Field.C a, Absolute.C a, Ord a) =>
   (a -> Int) -> (Int -> a) -> a -> Int
roundSimpleInt trunc _toFloat x =
   trunc (x + Absolute.signum x * 0.5)



{- RULES maybe used, when Prelude implementations become more efficient
     "NP.round    :: Float -> Int"    round    = P.round    :: Float -> Int;
     "NP.truncate :: Float -> Int"    truncate = P.truncate :: Float -> Int;
     "NP.floor    :: Float -> Int"    floor    = P.floor    :: Float -> Int;
     "NP.ceiling  :: Float -> Int"    ceiling  = P.ceiling  :: Float -> Int;
     "NP.round    :: Double -> Int"   round    = P.round    :: Double -> Int;
     "NP.truncate :: Double -> Int"   truncate = P.truncate :: Double -> Int;
     "NP.floor    :: Double -> Int"   floor    = P.floor    :: Double -> Int;
     "NP.ceiling  :: Double -> Int"   ceiling  = P.ceiling  :: Double -> Int;
  -}

-- these rules will also be needed for Int16 et.al.
{-# RULES
     "NP.round       :: Float -> Int"    round    = roundInt       GHC.float2Int  GHC.int2Float;
     "NP.roundSimple :: Float -> Int"    round    = roundSimpleInt GHC.float2Int  GHC.int2Float;
     "NP.truncate    :: Float -> Int"    truncate =                GHC.float2Int               ;
     "NP.floor       :: Float -> Int"    floor    = floorInt       GHC.float2Int  GHC.int2Float;
     "NP.ceiling     :: Float -> Int"    ceiling  = ceilingInt     GHC.float2Int  GHC.int2Float;
     "NP.round       :: Double -> Int"   round    = roundInt       GHC.double2Int GHC.int2Double;
     "NP.roundSimple :: Double -> Int"   round    = roundSimpleInt GHC.double2Int GHC.int2Double;
     "NP.truncate    :: Double -> Int"   truncate =                GHC.double2Int               ;
     "NP.floor       :: Double -> Int"   floor    = floorInt       GHC.double2Int GHC.int2Double;
     "NP.ceiling     :: Double -> Int"   ceiling  = ceilingInt     GHC.double2Int GHC.int2Double;

     "NP.splitFraction :: Float ->  (Int, Float)"  splitFraction = splitFractionInt GHC.float2Int GHC.int2Float;
     "NP.splitFraction :: Double -> (Int, Double)" splitFraction = splitFractionInt GHC.double2Int GHC.int2Double;
  #-}

-- generated by GenerateRules.hs
{-# RULES
     "NP.round       :: a -> Int8"    round       = (P.fromIntegral :: Int -> Int8) . round;
     "NP.roundSimple :: a -> Int8"    roundSimple = (P.fromIntegral :: Int -> Int8) . roundSimple;
     "NP.truncate    :: a -> Int8"    truncate    = (P.fromIntegral :: Int -> Int8) . truncate;
     "NP.floor       :: a -> Int8"    floor       = (P.fromIntegral :: Int -> Int8) . floor;
     "NP.ceiling     :: a -> Int8"    ceiling     = (P.fromIntegral :: Int -> Int8) . ceiling;
     "NP.round       :: a -> Int16"   round       = (P.fromIntegral :: Int -> Int16) . round;
     "NP.roundSimple :: a -> Int16"   roundSimple = (P.fromIntegral :: Int -> Int16) . roundSimple;
     "NP.truncate    :: a -> Int16"   truncate    = (P.fromIntegral :: Int -> Int16) . truncate;
     "NP.floor       :: a -> Int16"   floor       = (P.fromIntegral :: Int -> Int16) . floor;
     "NP.ceiling     :: a -> Int16"   ceiling     = (P.fromIntegral :: Int -> Int16) . ceiling;
     "NP.round       :: a -> Int32"   round       = (P.fromIntegral :: Int -> Int32) . round;
     "NP.roundSimple :: a -> Int32"   roundSimple = (P.fromIntegral :: Int -> Int32) . roundSimple;
     "NP.truncate    :: a -> Int32"   truncate    = (P.fromIntegral :: Int -> Int32) . truncate;
     "NP.floor       :: a -> Int32"   floor       = (P.fromIntegral :: Int -> Int32) . floor;
     "NP.ceiling     :: a -> Int32"   ceiling     = (P.fromIntegral :: Int -> Int32) . ceiling;
     "NP.round       :: a -> Int64"   round       = (P.fromIntegral :: Int -> Int64) . round;
     "NP.roundSimple :: a -> Int64"   roundSimple = (P.fromIntegral :: Int -> Int64) . roundSimple;
     "NP.truncate    :: a -> Int64"   truncate    = (P.fromIntegral :: Int -> Int64) . truncate;
     "NP.floor       :: a -> Int64"   floor       = (P.fromIntegral :: Int -> Int64) . floor;
     "NP.ceiling     :: a -> Int64"   ceiling     = (P.fromIntegral :: Int -> Int64) . ceiling;
     "NP.round       :: a -> Word"    round       = (P.fromIntegral :: Int -> Word) . round;
     "NP.roundSimple :: a -> Word"    roundSimple = (P.fromIntegral :: Int -> Word) . roundSimple;
     "NP.truncate    :: a -> Word"    truncate    = (P.fromIntegral :: Int -> Word) . truncate;
     "NP.floor       :: a -> Word"    floor       = (P.fromIntegral :: Int -> Word) . floor;
     "NP.ceiling     :: a -> Word"    ceiling     = (P.fromIntegral :: Int -> Word) . ceiling;
     "NP.round       :: a -> Word8"   round       = (P.fromIntegral :: Int -> Word8) . round;
     "NP.roundSimple :: a -> Word8"   roundSimple = (P.fromIntegral :: Int -> Word8) . roundSimple;
     "NP.truncate    :: a -> Word8"   truncate    = (P.fromIntegral :: Int -> Word8) . truncate;
     "NP.floor       :: a -> Word8"   floor       = (P.fromIntegral :: Int -> Word8) . floor;
     "NP.ceiling     :: a -> Word8"   ceiling     = (P.fromIntegral :: Int -> Word8) . ceiling;
     "NP.round       :: a -> Word16"  round       = (P.fromIntegral :: Int -> Word16) . round;
     "NP.roundSimple :: a -> Word16"  roundSimple = (P.fromIntegral :: Int -> Word16) . roundSimple;
     "NP.truncate    :: a -> Word16"  truncate    = (P.fromIntegral :: Int -> Word16) . truncate;
     "NP.floor       :: a -> Word16"  floor       = (P.fromIntegral :: Int -> Word16) . floor;
     "NP.ceiling     :: a -> Word16"  ceiling     = (P.fromIntegral :: Int -> Word16) . ceiling;
     "NP.round       :: a -> Word32"  round       = (P.fromIntegral :: Int -> Word32) . round;
     "NP.roundSimple :: a -> Word32"  roundSimple = (P.fromIntegral :: Int -> Word32) . roundSimple;
     "NP.truncate    :: a -> Word32"  truncate    = (P.fromIntegral :: Int -> Word32) . truncate;
     "NP.floor       :: a -> Word32"  floor       = (P.fromIntegral :: Int -> Word32) . floor;
     "NP.ceiling     :: a -> Word32"  ceiling     = (P.fromIntegral :: Int -> Word32) . ceiling;
     "NP.round       :: a -> Word64"  round       = (P.fromIntegral :: Int -> Word64) . round;
     "NP.roundSimple :: a -> Word64"  roundSimple = (P.fromIntegral :: Int -> Word64) . roundSimple;
     "NP.truncate    :: a -> Word64"  truncate    = (P.fromIntegral :: Int -> Word64) . truncate;
     "NP.floor       :: a -> Word64"  floor       = (P.fromIntegral :: Int -> Word64) . floor;
     "NP.ceiling     :: a -> Word64"  ceiling     = (P.fromIntegral :: Int -> Word64) . ceiling;

     "NP.splitFraction :: a -> (Int8,a)"     splitFraction = mapFst (P.fromIntegral :: Int -> Int8) . splitFraction;
     "NP.splitFraction :: a -> (Int16,a)"    splitFraction = mapFst (P.fromIntegral :: Int -> Int16) . splitFraction;
     "NP.splitFraction :: a -> (Int32,a)"    splitFraction = mapFst (P.fromIntegral :: Int -> Int32) . splitFraction;
     "NP.splitFraction :: a -> (Int64,a)"    splitFraction = mapFst (P.fromIntegral :: Int -> Int64) . splitFraction;
     "NP.splitFraction :: a -> (Word,a)"     splitFraction = mapFst (P.fromIntegral :: Int -> Word) . splitFraction;
     "NP.splitFraction :: a -> (Word8,a)"    splitFraction = mapFst (P.fromIntegral :: Int -> Word8) . splitFraction;
     "NP.splitFraction :: a -> (Word16,a)"   splitFraction = mapFst (P.fromIntegral :: Int -> Word16) . splitFraction;
     "NP.splitFraction :: a -> (Word32,a)"   splitFraction = mapFst (P.fromIntegral :: Int -> Word32) . splitFraction;
     "NP.splitFraction :: a -> (Word64,a)"   splitFraction = mapFst (P.fromIntegral :: Int -> Word64) . splitFraction;
  #-}


{- | TODO: Should be moved to a continued fraction module. -}

approxRational :: (ToRational.C a, C a) => a -> a -> Rational
approxRational rat eps    =  simplest (rat-eps) (rat+eps)
        where simplest x y | y < x      =  simplest y x
                           | x == y     =  xr
                           | x > 0      =  simplest' n d n' d'
                           | y < 0      =  - simplest' (-n') d' (-n) d
                           | otherwise  =  0 :% 1
                                        where xr@(n:%d) = ToRational.toRational x
                                              (n':%d')  = ToRational.toRational y

              simplest' n d n' d'       -- assumes 0 < n%d < n'%d'
                        | isZero r   =  q :% 1
                        | q /= q'    =  (q+1) :% 1
                        | otherwise  =  (q*n''+d'') :% n''
                                     where (q,r)      =  quotRem n d
                                           (q',r')    =  quotRem n' d'
                                           (n'':%d'') =  simplest' d' r' d r


-- * generic implementation of round functions

powersOfTwo :: (Ring.C a) => [a]
powersOfTwo = iterate (2*) one

pairsOfPowersOfTwo :: (Ring.C a, Ring.C b) => [(a,b)]
pairsOfPowersOfTwo =
   zip powersOfTwo powersOfTwo

{- |
The generic rounding functions need a number of operations
proportional to the number of binary digits of the integer portion.
If operations like multiplication with two and comparison
need time proportional to the number of binary digits,
then the overall rounding requires quadratic time.

prop> RealRing.genericFloor =~= (NP.floor :: Double -> Integer)
prop> RealRing.genericFloor =~= (NP.floor :: Rational -> Integer)
-}
genericFloor :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericFloor a =
   if a>=zero
     then genericPosFloor a
     else negate $ genericPosCeiling $ negate a

{- |
prop> RealRing.genericCeiling =~= (NP.ceiling :: Double -> Integer)
prop> RealRing.genericCeiling =~= (NP.ceiling :: Rational -> Integer)
-}
genericCeiling :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericCeiling a =
   if a>=zero
     then genericPosCeiling a
     else negate $ genericPosFloor $ negate a

{- |
prop> RealRing.genericTruncate =~= (NP.truncate :: Double -> Integer)
prop> RealRing.genericTruncate =~= (NP.truncate :: Rational -> Integer)
-}
genericTruncate :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericTruncate a =
   if a>=zero
     then genericPosFloor a
     else negate $ genericPosFloor $ negate a

{- |
prop> RealRing.genericRound =~= (NP.round :: Double -> Integer)
prop> RealRing.genericRound =~= (NP.round :: Rational -> Integer)
-}
genericRound :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericRound a =
   if a>=zero
     then genericPosRound a
     else negate $ genericPosRound $ negate a

{- |
prop> RealRing.genericFraction =~= (NP.fraction :: Double -> Double)
prop> RealRing.genericFraction =~= (NP.fraction :: Rational -> Rational)
-}
genericFraction :: (Ord a, Ring.C a) => a -> a
genericFraction a =
   if a>=zero
     then genericPosFraction a
     else fixFraction $ negate $ genericPosFraction $ negate a

{- |
prop> RealRing.genericSplitFraction =~= (NP.splitFraction :: Double -> (Integer,Double))
prop> RealRing.genericSplitFraction =~= (NP.splitFraction :: Rational -> (Integer,Rational))
-}
genericSplitFraction :: (Ord a, Ring.C a, Ring.C b) => a -> (b,a)
genericSplitFraction a =
   if a>=zero
     then genericPosSplitFraction a
     else fixSplitFraction $ mapPair (negate, negate) $
          genericPosSplitFraction $ negate a


genericPosFloor :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericPosFloor a =
   snd $
   foldr
      (\(pa,pb) acc@(accA,accB) ->
         let newA = accA+pa
         in  if newA>a then acc else (newA,accB+pb))
      (zero,zero) $
   takeWhile ((a>=) . fst) $
   pairsOfPowersOfTwo

genericPosCeiling :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericPosCeiling a =
   snd $
   (\(ps,u:_) ->
      foldr
         (\(pa,pb) acc@(accA,accB) ->
            let newA = accA-pa
            in  if newA>=a then (newA,accB-pb) else acc)
         u ps) $
   span ((a>) . fst) $
   (zero,zero) : pairsOfPowersOfTwo

{-
genericPosFloorDigits :: (Ord a, Ring.C a, Ring.C b) => a -> ((a,b), [Bool])
genericPosFloorDigits a =
   List.mapAccumR
      (\acc@(accA,accB) (pa,pb) ->
         let newA = accA+pa
             b = newA<=a
         in  (if b then (newA,accB+pb) else acc, b))
      (zero,zero) $
   takeWhile ((a>=) . fst) $
   pairsOfPowersOfTwo
-}

genericHalfPosFloorDigits :: (Ord a, Ring.C a, Ring.C b) => a -> ((a,b), [Bool])
genericHalfPosFloorDigits a =
   List.mapAccumR
      (\acc@(accA,accB) (pa,pb) ->
         let newA = accA+pa
             b = newA<=a
         in  (if b then (newA,accB+pb) else acc, b))
      (zero,zero) $
   takeWhile ((a>=) . fst) $
   zip powersOfTwo (zero:powersOfTwo)

genericPosRound :: (Ord a, Ring.C a, Ring.C b) => a -> b
genericPosRound a =
   let a2 = 2*a
       ((ai,bi), ds) = genericHalfPosFloorDigits a2
   in  if ai==a2
         then
           case ds of
             True : True : _ -> bi+one
             _ -> bi
         else
           case ds of
             True : _ -> bi+one
             _ -> bi

genericPosFraction :: (Ord a, Ring.C a) => a -> a
genericPosFraction a =
   foldr
      (\p acc ->
         if p>acc then acc else acc-p)
      a $
   takeWhile (a>=) $
   powersOfTwo

genericPosSplitFraction :: (Ord a, Ring.C a, Ring.C b) => a -> (b,a)
genericPosSplitFraction a =
   foldr
      (\(pb,pa) acc@(accB,accA) ->
         if pa>accA then acc else (accB+pb,accA-pa))
      (zero,a) $
   takeWhile ((a>=) . snd) $
   pairsOfPowersOfTwo


{- |
Needs linear time with respect to the number of digits.

This and other functions using OrderDecision
like @floor@ where argument and result are the same
may be moved to a new module.
-}
decisionPosFraction :: (OrdDec.C a, Ring.C a) => a -> a
decisionPosFraction a0 =
   (\ps ->
      foldr
         (\p cont a ->
            (a<?one) a $ cont $
            (a>=?p) (a-p) a)
         (error "decisionPosFraction: end of list should never be reached")
         ps a0) $
   concatMap (reverse . flip take powersOfTwo) powersOfTwo

{-
Works but needs quadratic time with respect to the number of digits.
I feel that there must be something more efficient.
-}
decisionPosFractionSqrTime :: (OrdDec.C a, Ring.C a) => a -> a
decisionPosFractionSqrTime a0 =
   (\ps ->
      foldr
         (\p cont a ->
            (a<?one) a $ cont $
            (a>=?p) (a-p) a)
         (error "decisionPosFraction: end of list should never be reached")
         ps a0) $
   concatMap reverse $
   inits powersOfTwo
