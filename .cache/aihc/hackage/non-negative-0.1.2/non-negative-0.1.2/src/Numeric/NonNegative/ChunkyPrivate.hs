{- |
Copyright   :  (c) Henning Thielemann 2008-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98

This module contains internal functions (*Unsafe)
that I had liked to re-use in the NumericPrelude type hierarchy.
However since the Eq and Ord instance already require the Num class,
we cannot use that in the NumericPrelude.
-}
module Numeric.NonNegative.ChunkyPrivate
   (T, fromChunks, fromNumber, toChunks, toNumber,
    zero, normalize, isNull, isPositive,
    divModStrict,
    fromChunksUnsafe, toChunksUnsafe, ) where

import qualified Numeric.NonNegative.Class as NonNeg
import Control.Monad (liftM, liftM2)
import Data.Monoid (Monoid(mempty, mappend), )
import Data.Semigroup (Semigroup((<>)), )
import Data.Tuple.HT (mapSnd, )

import Test.QuickCheck (Arbitrary(arbitrary, shrink))

{- |
A chunky non-negative number is a list of non-negative numbers.
It represents the sum of the list elements.
It is possible to represent a finite number with infinitely many chunks
by using an infinite number of zeros.

Note the following problems:

Addition is commutative only for finite representations.
E.g. @let y = min (1+y) 2 in y@ is defined,
@let y = min (y+1) 2 in y@ is not.
-}
newtype T a = Cons {decons :: [a]}


fromChunks :: NonNeg.C a => [a] -> T a
fromChunks = Cons

fromNumber :: NonNeg.C a => a -> T a
fromNumber = fromChunks . (:[])


{- |
This routine exposes the inner structure of the lazy number.
-}
toChunks :: T a -> [a]
toChunks = decons

toNumber :: NonNeg.C a => T a -> a
toNumber =  NonNeg.sum . decons


instance (Show a) => Show (T a) where
   showsPrec p x =
      showParen (p>10)
         (showString "Chunky.fromChunks " . showsPrec 10 (decons x))


lift2 :: ([a] -> [a] -> [a]) -> (T a -> T a -> T a)
lift2 f (Cons x) (Cons y) = Cons $ f x y

zero :: T a
zero = Cons []

{- |
Remove zero chunks.
-}
normalize :: NonNeg.C a => T a -> T a
normalize = Cons . filter (> NonNeg.zero) . decons

isNullList :: NonNeg.C a => [a] -> Bool
isNullList = null . filter (> NonNeg.zero)

isNull :: NonNeg.C a => T a -> Bool
isNull = isNullList . decons
  -- null . decons . normalize

isPositive :: NonNeg.C a => T a -> Bool
isPositive = not . isNull

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


{- |
This instance is not correct with respect to the equality check
if the involved numbers contain zero chunks.
-}
instance (NonNeg.C a) => NonNeg.C (T a) where
   split (Cons xs) (Cons ys) =
      let (zs, ~(b, rs)) = glue xs ys
      in  (Cons zs, (b, Cons rs))
{-
   (Cons x) -| (Cons w) =
      let sub _ [] = []
          sub z (y:ys) =
             if z<y then (y-|z):ys else sub (z-|y) ys
      in  Cons (foldr sub x w)
-}

instance (NonNeg.C a, Num a) => Num (T a) where
   (+) = mappend
   (Cons x) - (Cons y) =
      let (b,d) = snd $ glue x y
          d' = Cons d
      in check "-" (not b || isNull d') d'
   negate x = check "negate" (isNull x) x
   fromInteger = fromNumber . fromInteger
   (*)    = lift2 (liftM2 (*))
   abs    = id
   signum = fromNumber . (\b -> if b then 1 else 0) . isPositive


instance (Real a, NonNeg.C a) => Real (T a) where
   toRational = toRational . toNumber

{- required for Integral instance -}
instance (Enum a, NonNeg.C a) => Enum (T a) where
   toEnum   = fromNumber . toEnum
   fromEnum = fromEnum . toNumber

instance (Integral a, NonNeg.C a) => Integral (T a) where
   toInteger = toInteger . toNumber
   quot = div
   rem  = mod
   quotRem = divMod
   divMod x y =
      mapSnd fromNumber $
      divModStrict x (toNumber y)

divModStrict ::
   (Integral a, NonNeg.C a) =>
   T a -> a -> (T a, a)
divModStrict x0 y =
   let recourse [] r = ([], r)
       recourse (x:xs) r0 =
          let (q1,r1) = divMod (x+r0) y
              (q2,r2) = recourse xs r1
          in  (q1:q2,r2)
       (cs,rm) = recourse (toChunks x0) 0
   in  (fromChunks cs, rm)


instance Semigroup (T a) where
   (<>) = lift2 (++)

instance Monoid (T a) where
   mempty = zero
   mappend = lift2 (++)

instance (NonNeg.C a, Arbitrary a) => Arbitrary (T a) where
   arbitrary = liftM Cons arbitrary
   shrink (Cons xs) = map Cons $ shrink xs


{- * Functions that may break invariants -}

fromChunksUnsafe :: [a] -> T a
fromChunksUnsafe = Cons

{- |
This routine exposes the inner structure of the lazy number
and is actually the same as 'toChunks'.
It was considered dangerous,
but you can observe the lazy structure
in tying-the-knot applications anyway.
So the explicit revelation of the chunks seems not to be worse.
-}
toChunksUnsafe :: T a -> [a]
toChunksUnsafe = decons
