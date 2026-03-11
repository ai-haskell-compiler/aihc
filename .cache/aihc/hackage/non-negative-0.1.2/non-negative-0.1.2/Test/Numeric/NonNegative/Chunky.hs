{- |
Copyright   :  (c) Henning Thielemann 2007-2010

Maintainer  :  haskell@henning-thielemann.de
Stability   :  stable
Portability :  Haskell 98
-}
module Test.Numeric.NonNegative.Chunky (tests) where

-- import Test.Utility
import Test.QuickCheck (quickCheck, Property, (==>))

import qualified Numeric.NonNegative.Chunky  as Chunky
import qualified Numeric.NonNegative.Class   as NonNeg
import qualified Numeric.NonNegative.Wrapper as NonNegW
-- import Numeric.NonNegative.Class ((-|))

-- import Debug.Trace (trace)

import qualified Prelude as P
import Prelude hiding (min, max, abs, signum, compare)


infixl 5 $~

($~) :: NonNeg.C a =>
   (a -> b) -> (Chunky.T a -> b)
($~) f = f . Chunky.toNumber

infixl 4 ==~

(==~) :: NonNeg.C a =>
   Chunky.T a -> a -> Bool
(==~) xs ys =
   Chunky.toNumber xs == ys



add :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
add x y  =  x+y  ==~  (+) $~ x $~ y

sub :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
sub x y  =
   if x>y
     then x-y  ==~  (-) $~ x $~ y
     else y-x  ==~  (-) $~ y $~ x

mul :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
mul x y  =  x*y  ==~  (*) $~ x $~ y

min :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
min x y  =  P.min x y  ==~  P.min $~ x $~ y

max :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
max x y  =  P.max x y  ==~  P.max $~ x $~ y

abs :: (NonNeg.C a, Num a) => Chunky.T a -> Bool
abs x  =  P.abs x  ==~  P.abs $~ x

signum :: (NonNeg.C a, Num a) => Chunky.T a -> Bool
signum x  =  P.signum x  ==~  P.signum $~ x

equal :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
equal x y  =  (x == y)  ==  (==) $~ x $~ y

compare :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Bool
compare x y  =  P.compare x y  ==  P.compare $~ x $~ y

splitSpaceLeak0, splitSpaceLeak1, splitSpaceLeak2,
 splitSpaceLeak3, splitSpaceLeak4, splitSpaceLeak5 :: Bool
splitSpaceLeak0 =
   (\t -> t==t) $ take 300 $
   show $ fst $ NonNeg.split 1000000 $
   Chunky.fromChunks $ repeat (1::Card)
splitSpaceLeak1 =
   (\t -> t==t) $ take 300 $
   show $ fst $ snd $ NonNeg.split 1000000 $
   Chunky.fromChunks $ repeat (1::Card)
splitSpaceLeak2 =
   (\t -> t==t) $ take 300 $
   show $ snd $ snd $ NonNeg.split 1000000 $
   Chunky.fromChunks $ repeat (1::Card)
splitSpaceLeak3 =
   (\t -> t==t) $ take 300 $
   show $ snd $ NonNeg.split 1000000 $
   Chunky.fromChunks $ repeat (1::Card)
splitSpaceLeak4 =
   (\t -> t==t) $ take 300 $
   show $ (\ ~(_mt,bdt) -> bdt) $ NonNeg.split 1000000 $
   Chunky.fromChunks $ repeat (1::Card)
splitSpaceLeak5 =
   (\t -> t==t) $ take 300 $
   show $ (\(b,dt) -> if b then dt else 0) $ snd $ NonNeg.split 1000000 $
   Chunky.fromChunks $ repeat (1::Card)

infinity :: (NonNeg.C a, Num a) => a -> Chunky.T a
infinity = Chunky.fromChunks . repeat . (1+)

checkInfinity :: (NonNeg.C a, Num a) => a -> Chunky.T a -> Bool
checkInfinity limit x =
   let y = Chunky.fromNumber limit
   in  P.min x y == y

addInfiniteL :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
addInfiniteL limit x y =
   checkInfinity limit (infinity x + y)

addInfiniteR :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
addInfiniteR limit x y =
   checkInfinity limit (y + infinity x)


mulInfiniteL :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Property
mulInfiniteL limit x y =
   Chunky.isPositive y ==>
      checkInfinity limit (infinity x * y)

{- |
Without normalization the test would fail for
@y = Chunky.fromChunks [0,1]@
-}
mulInfiniteR :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Property
mulInfiniteR limit x y =
   Chunky.isPositive y ==>
      checkInfinity limit (Chunky.normalize y * infinity x)

{-
mulInfiniteL :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
mulInfiniteL limit x y =
   if Chunky.isNull y
     then Chunky.isNull (infinity x * y)
     else checkInfinity limit (infinity x * y)

mulInfiniteR :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
mulInfiniteR limit x y =
   if Chunky.isNull y
     then Chunky.isNull (y * infinity x)
     else checkInfinity limit (y * infinity x)
-}

minRecursiveL :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Property
minRecursiveL x y =
   Chunky.isPositive x ==>
      let z = P.min (x+z) y
      in  z == y

minRecursiveR :: (NonNeg.C a, Num a) => Chunky.T a -> Chunky.T a -> Property
minRecursiveR x y =
   Chunky.isPositive x ==>
      let z = P.min y (x+z)
      in  z == y

minInfiniteL :: (NonNeg.C a, Num a) => a -> Chunky.T a -> Bool
minInfiniteL x y =
   P.min (infinity x) y  ==  y

minInfiniteR :: (NonNeg.C a, Num a) => a -> Chunky.T a -> Bool
minInfiniteR x y =
   P.min y (infinity x)  ==  y

maxInfiniteL :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
maxInfiniteL limit x y =
   checkInfinity limit (P.max (infinity x) y)

maxInfiniteR :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
maxInfiniteR limit x y =
   checkInfinity limit (P.max y (infinity x))

minInfiniteAssociative :: (NonNeg.C a, Num a) => a -> a -> Chunky.T a -> Bool
minInfiniteAssociative x y z =
   P.min (P.min (infinity x) (infinity y)) z  ==  z

maxInfiniteAssociative :: (NonNeg.C a, Num a) => a -> a -> a -> Chunky.T a -> Bool
maxInfiniteAssociative limit x y z =
   checkInfinity limit
      (P.max (P.max (infinity x) (infinity y)) z)

compareInfiniteL :: (NonNeg.C a, Num a) => a -> Chunky.T a -> Bool
compareInfiniteL x y =
   P.compare (infinity x) y == GT

compareInfiniteR :: (NonNeg.C a, Num a) => a -> Chunky.T a -> Bool
compareInfiniteR x y =
   P.compare y (infinity x) == LT





type Card = NonNegW.Integer

tests :: [(String, IO ())]
tests =
   ("add",
     quickCheck (add :: Chunky.T Card -> Chunky.T Card -> Bool)) :
   ("sub",
     quickCheck (sub :: Chunky.T Card -> Chunky.T Card -> Bool)) :
   ("mul",
     quickCheck (mul :: Chunky.T Card -> Chunky.T Card -> Bool)) :
   ("min",
     quickCheck (min :: Chunky.T Card -> Chunky.T Card -> Bool)) :
   ("max",
     quickCheck (max :: Chunky.T Card -> Chunky.T Card -> Bool)) :
   ("abs",
     quickCheck (abs :: Chunky.T Card -> Bool)) :
   ("signum",
     quickCheck (signum :: Chunky.T Card -> Bool)) :
   ("equal",
     quickCheck (equal :: Chunky.T Card -> Chunky.T Card -> Bool)) :
   ("compare",
     quickCheck (compare :: Chunky.T Card -> Chunky.T Card -> Bool)) :

   ("splitSpaceLeak0",
     quickCheck splitSpaceLeak0) :
   ("splitSpaceLeak1",
     quickCheck splitSpaceLeak1) :
   ("splitSpaceLeak2",
     quickCheck splitSpaceLeak2) :
   ("splitSpaceLeak3",
     quickCheck splitSpaceLeak3) :
   ("splitSpaceLeak4",
     quickCheck splitSpaceLeak4) :
   ("splitSpaceLeak5",
     quickCheck splitSpaceLeak5) :

   ("addInfiniteL",
     quickCheck (addInfiniteL :: Card -> Card -> Chunky.T Card -> Bool)) :
   ("addInfiniteR",
     quickCheck (addInfiniteR :: Card -> Card -> Chunky.T Card -> Bool)) :
   ("mulInfiniteL",
     quickCheck (mulInfiniteL :: Card -> Card -> Chunky.T Card -> Property)) :
   ("mulInfiniteR",
     quickCheck (mulInfiniteR :: Card -> Card -> Chunky.T Card -> Property)) :
   ("minRecursiveL",
     quickCheck (minRecursiveL :: Chunky.T Card -> Chunky.T Card -> Property)) :
   ("minRecursiveR",
     quickCheck (minRecursiveR :: Chunky.T Card -> Chunky.T Card -> Property)) :
   ("minInfiniteL",
     quickCheck (minInfiniteL :: Card -> Chunky.T Card -> Bool)) :
   ("minInfiniteR",
     quickCheck (minInfiniteR :: Card -> Chunky.T Card -> Bool)) :
   ("maxInfiniteL",
     quickCheck (maxInfiniteL :: Card -> Card -> Chunky.T Card -> Bool)) :
   ("maxInfiniteR",
     quickCheck (maxInfiniteR :: Card -> Card -> Chunky.T Card -> Bool)) :
   ("minInfiniteAssociative",
     quickCheck (minInfiniteAssociative :: Card -> Card -> Chunky.T Card -> Bool)) :
   ("maxInfiniteAssociative",
     quickCheck (maxInfiniteAssociative :: Card -> Card -> Card -> Chunky.T Card -> Bool)) :

   ("compareInfiniteL",
     quickCheck (compareInfiniteL :: Card -> Chunky.T Card -> Bool)) :
   ("compareInfiniteR",
     quickCheck (compareInfiniteR :: Card -> Chunky.T Card -> Bool)) :
   []
