{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

A lazy number type, which is a generalization of lazy Peano numbers.
Comparisons can be made lazy and
thus computations are possible which are impossible with strict number types,
e.g. you can compute @let y = min (1+y) 2 in y@.
You can even work with infinite values.
However, depending on the granularity,
the memory consumption is higher than that for strict number types.
This number type is of interest for the merge operation of event lists,
which allows for co-recursive merges.
-}
module Number.NonNegativeChunky
   (T, fromChunks, toChunks, fromNumber, toNumber, fromChunky98, toChunky98,
    minMaxDiff, normalize, isNull, isPositive,
    divModLazy, divModStrict, ) where

import qualified Numeric.NonNegative.Chunky as Chunky98
import qualified Numeric.NonNegative.Class as NonNeg98

import qualified Algebra.NonNegative  as NonNeg
import qualified Algebra.Absolute     as Absolute
import qualified Algebra.Ring         as Ring
import qualified Algebra.Additive     as Additive
import qualified Algebra.ToInteger    as ToInteger
import qualified Algebra.ToRational   as ToRational
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.RealIntegral as RealIntegral
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Algebra.Monoid as Monoid
import qualified Data.Monoid as Mn98
import qualified Data.Semigroup as Sg98

import Control.Monad (liftM, liftM2, )
import Data.Tuple.HT (mapFst, mapSnd, mapPair, )

import Test.QuickCheck (Arbitrary(arbitrary))

import NumericPrelude.Numeric
import NumericPrelude.Base

import qualified Prelude as P98


{- |
A chunky non-negative number is a list of non-negative numbers.
It represents the sum of the list elements.
It is possible to represent a finite number with infinitely many chunks
by using an infinite number of zeros.

Note the following problems:

Addition is commutative only for finite representations.
E.g. @let y = min (1+y) 2 in y@ is defined,
@let y = min (y+1) 2 in y@ is not.

The type is equivalent to 'Numeric.NonNegative.Chunky'.
-}
newtype T a = Cons {decons :: [a]}


fromChunks :: NonNeg.C a => [a] -> T a
fromChunks = Cons

toChunks :: NonNeg.C a => T a -> [a]
toChunks = decons

fromChunky98 :: (NonNeg.C a, NonNeg98.C a) => Chunky98.T a -> T a
fromChunky98 = fromChunks . Chunky98.toChunks

toChunky98 :: (NonNeg.C a, NonNeg98.C a) => T a -> Chunky98.T a
toChunky98 = Chunky98.fromChunks . toChunks

fromNumber :: NonNeg.C a => a -> T a
fromNumber = fromChunks . (:[])

toNumber :: NonNeg.C a => T a -> a
toNumber =  Monoid.cumulate . toChunks



lift2 :: NonNeg.C a => ([a] -> [a] -> [a]) -> (T a -> T a -> T a)
lift2 f x y =
   fromChunks $ f (toChunks x) (toChunks y)

{- |
Remove zero chunks.
-}
normalize :: NonNeg.C a => T a -> T a
normalize = fromChunks . filter (> NonNeg.zero) . toChunks

isNullList :: NonNeg.C a => [a] -> Bool
isNullList = null . filter (> NonNeg.zero)

isNull :: NonNeg.C a => T a -> Bool
isNull = isNullList . toChunks
  -- null . toChunks . normalize

isPositive :: NonNeg.C a => T a -> Bool
isPositive = not . isNull



{-
normalizeZT :: ZeroTestable.C a => T a -> T a
normalizeZT = fromChunks . filter (not . isZero) . toChunks
-}

isNullListZT :: ZeroTestable.C a => [a] -> Bool
isNullListZT = null . filter (not . isZero)

isNullZT :: ZeroTestable.C a => T a -> Bool
isNullZT = isNullListZT . decons
  -- null . toChunks . normalize
{-
isPositiveZT :: ZeroTestable.C a => T a -> Bool
isPositiveZT = not . isNull
-}


check :: String -> Bool -> a -> a
check funcName b x =
   if b
     then x
     else error ("Numeric.NonNegative.Chunky."++funcName++": negative number")


glue :: (NonNeg.C a) => [a] -> [a] -> ([a], (Bool, [a]))
glue [] ys = ([], (True,  ys))
glue xs [] = ([], (False, xs))
glue (x:xs) (y:ys) =
   let (z,~(zs,brs)) =
          flip mapSnd (NonNeg.split x y) $
          \(b,d) ->
             if b
               then glue xs $
                    if NonNeg.zero == d
                      then ys else d:ys
               else glue (d:xs) ys
   in  (z:zs,brs)

minMaxDiff :: (NonNeg.C a) => T a -> T a -> (T a, (Bool, T a))
minMaxDiff (Cons xs) (Cons ys) =
   let (zs, (b, rs)) = glue xs ys
   in  (Cons zs, (b, Cons rs))

equalList :: (NonNeg.C a) => [a] -> [a] -> Bool
equalList x y =
   isNullList $ snd $ snd $ glue x y

compareList :: (NonNeg.C a) => [a] -> [a] -> Ordering
compareList x y =
   let (b,r) = snd $ glue x y
   in  if isNullList r
         then EQ
         else if b then LT else GT

minList :: (NonNeg.C a) => [a] -> [a] -> [a]
minList x y =
   fst $ glue x y

maxList :: (NonNeg.C a) => [a] -> [a] -> [a]
maxList x y =
   -- matching the inner pair lazily is important
   let (z,~(_,r)) = glue x y in z++r


instance (NonNeg.C a) => Eq (T a) where
   (Cons x) == (Cons y) = equalList x y

instance (NonNeg.C a) => Ord (T a) where
   compare (Cons x) (Cons y) = compareList x y
   min = lift2 minList
   max = lift2 maxList


instance (NonNeg.C a) => NonNeg.C (T a) where
   split (Cons xs) (Cons ys) =
      let (zs, ~(b, rs)) = glue xs ys
      in  (Cons zs, (b, Cons rs))

instance (ZeroTestable.C a) => ZeroTestable.C (T a) where
   isZero = isNullZT

instance (NonNeg.C a) => Additive.C (T a) where
   zero  = Monoid.idt
   (+)   = (Monoid.<*>)
   (Cons x) - (Cons y) =
      let (b,d) = snd $ glue x y
          d' = Cons d
      in check "-" (not b || isNull d') d'
   negate x = check "negate" (isNull x) x
{-
   x0 - y0 =
      let d' = lift2 (\x y -> let (_,d,b) = glue x y in  d) x0 y0
      in  check "-" (not b || isNull d') d'
-}

instance (Ring.C a, NonNeg.C a) => Ring.C (T a) where
   one   = fromNumber one
   (*)   = lift2 (liftM2 (*))
   fromInteger = fromNumber . fromInteger

instance (Ring.C a, ZeroTestable.C a, NonNeg.C a) => Absolute.C (T a) where
   abs    = id
   signum = fromNumber . (\b -> if b then one else zero) . isPositive

instance (ToInteger.C a, NonNeg.C a) => ToInteger.C (T a) where
   toInteger = sum . map toInteger . toChunks

instance (ToRational.C a, NonNeg.C a) => ToRational.C (T a) where
   toRational = sum . map toRational . toChunks


instance (RealIntegral.C a, NonNeg.C a) => RealIntegral.C (T a) where
   quot = div
   rem  = mod
   quotRem = divMod

{- |
'divMod' is implemented in terms of 'divModStrict'.
If it is needed we could also provide a function
that accesses the divisor first in a lazy way
and then uses a strict divisor for subsequent rounds of the subtraction loop.
This way we can handle the cases \"dividend smaller than divisor\"
and \"dividend greater than divisor\" in a lazy and efficient way.
However changing the way of operation within one number is also not nice.
-}
instance (Ord a, Integral.C a, NonNeg.C a) => Integral.C (T a) where
   divMod x y =
      mapSnd fromNumber $
      divModStrict x (toNumber y)

{- |
divModLazy accesses the divisor in a lazy way.
However this is only relevant if the dividend is smaller than the divisor.
For large dividends the divisor will be accessed multiple times
but since it is already fully evaluated it could also be strict.
-}
divModLazy ::
   (Ring.C a, NonNeg.C a) =>
   T a -> T a -> (T a, T a)
divModLazy x0 y0 =
   let y = toChunks y0
       recourse x =
          let (r,~(b,d)) = glue y x
          in  if not b
                then ([], r)
                else mapFst (one:) (recourse d)
   in  mapPair
          (fromChunks, fromChunks)
          (recourse (toChunks x0))

{- |
This function has a strict divisor
and maintains the chunk structure of the dividend at a smaller scale.
-}
divModStrict ::
   (Integral.C a, NonNeg.C a) =>
   T a -> a -> (T a, a)
divModStrict x0 y =
   let recourse [] r = ([], r)
       recourse (x:xs) r0 =
          case divMod (x+r0) y of
             (q,r1) -> mapFst (q:) $ recourse xs r1
   in  mapFst fromChunks $ recourse (toChunks x0) zero



instance (Show a) => Show (T a) where
   showsPrec p x =
      showParen (p>10)
         (showString "Chunky.fromChunks " . showsPrec 10 (decons x))


instance (NonNeg.C a, Arbitrary a) => Arbitrary (T a) where
   arbitrary = liftM Cons arbitrary



-- * Haskell 98 legacy instances

fromChunky98_ :: (NonNeg98.C a) => Chunky98.T a -> T a
fromChunky98_ = Cons . Chunky98.toChunks

toChunky98_ :: (NonNeg98.C a) => T a -> Chunky98.T a
toChunky98_ = Chunky98.fromChunks . decons

fromNumber_ :: a -> T a
fromNumber_ = Cons . (:[])

{-# INLINE lift98_1 #-}
lift98_1 ::
   (NonNeg98.C a, NonNeg98.C b) =>
   (Chunky98.T a -> Chunky98.T b) -> T a -> T b
lift98_1 f a = fromChunky98_ (f (toChunky98_ a))

{-# INLINE lift98_2 #-}
lift98_2 ::
   (NonNeg98.C a, NonNeg98.C b, NonNeg98.C c) =>
   (Chunky98.T a -> Chunky98.T b -> Chunky98.T c) -> T a -> T b -> T c
lift98_2 f a b = fromChunky98_ (f (toChunky98_ a) (toChunky98_ b))


{-# INLINE notImplemented #-}
notImplemented :: String -> a
notImplemented name =
   error $ "Number.NonNegativeChunky: method " ++ name ++ " cannot be implemented"

instance (NonNeg98.C a, P98.Num a) => P98.Num (T a) where
   fromInteger = fromNumber_ . P98.fromInteger
   negate = lift98_1 P98.negate
   (+)    = lift98_2 (P98.+)
   (*)    = lift98_2 (P98.*)
   abs    = lift98_1 P98.abs
   signum = lift98_1 P98.signum

instance (NonNeg98.C a, P98.Fractional a) => P98.Fractional (T a) where
   fromRational = fromNumber_ . P98.fromRational
   (/) = notImplemented "(/)"

instance (NonNeg.C a) => Sg98.Semigroup (T a) where
   (<>) = (Monoid.<*>)

instance (NonNeg.C a) => Mn98.Monoid (T a) where
   mempty  = Monoid.idt
   mappend = (Sg98.<>)

instance (NonNeg.C a) => Monoid.C (T a) where
   idt   = Cons []
   (<*>) = lift2 (++)
