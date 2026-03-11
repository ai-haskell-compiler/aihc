{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright   :  (c) Henning Thielemann 2004-2005

Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Computations on the set of roots of a polynomial.
These are represented as the list of their elementar symmetric terms.
The difference between a polynomial and the list of elementar symmetric terms
is the reversed order and the alternated signs.

Cf. /MathObj.PowerSum/ .
-}
module MathObj.RootSet where

import qualified MathObj.Polynomial      as Poly
import qualified MathObj.Polynomial.Core as PolyCore
import qualified MathObj.PowerSum        as PowerSum

import qualified Algebra.Algebraic    as Algebraic
import qualified Algebra.IntegralDomain as Integral
import qualified Algebra.Field        as Field
import qualified Algebra.Ring         as Ring
import qualified Algebra.Additive     as Additive
import qualified Algebra.ZeroTestable as ZeroTestable

import qualified Algebra.RealRing     as RealRing

import qualified Data.List.Match as Match
import qualified Data.List.Key as Key
import Control.Monad (liftM2, replicateM, )

import NumericPrelude.Base as P hiding (const)
import NumericPrelude.Numeric as NP


newtype T a = Cons {coeffs :: [a]}


{- * Conversions -}

lift0 :: [a] -> T a
lift0 = Cons

lift1 :: ([a] -> [a]) -> (T a -> T a)
lift1 f (Cons x0) = Cons (f x0)

lift2 :: ([a] -> [a] -> [a]) -> (T a -> T a -> T a)
lift2 f (Cons x0) (Cons x1) = Cons (f x0 x1)


const :: (Ring.C a) => a -> T a
const x = Cons [1,x]


toPolynomial :: T a -> Poly.T a
toPolynomial (Cons xs) = Poly.fromCoeffs (reverse xs)

fromPolynomial :: Poly.T a -> T a
fromPolynomial xs = Cons (reverse (Poly.coeffs xs))



toPowerSums :: (Field.C a, ZeroTestable.C a) => [a] -> [a]
toPowerSums = PowerSum.fromElemSymDenormalized

fromPowerSums :: (Field.C a, ZeroTestable.C a) => [a] -> [a]
fromPowerSums = PowerSum.toElemSym


{- | cf. 'MathObj.Polynomial.mulLinearFactor' -}
addRoot :: Ring.C a => a -> [a] -> [a]
addRoot x yt@(y:ys) =
   y : (ys + PolyCore.scale x yt)
addRoot _ [] =
   error "addRoot: list of elementar symmetric terms must consist at least of a 1"

fromRoots :: Ring.C a => [a] -> [a]
fromRoots = foldl (flip addRoot) [1]



liftPowerSum1Gen :: ([a] -> [a]) -> ([a] -> [a]) ->
   ([a] -> [a]) -> ([a] -> [a])
liftPowerSum1Gen fromPS toPS op x =
   Match.take x (fromPS (op (toPS x)))

liftPowerSum2Gen :: ([a] -> [a]) -> ([a] -> [a]) ->
   ([a] -> [a] -> [a]) -> ([a] -> [a] -> [a])
liftPowerSum2Gen fromPS toPS op x y =
   Match.take (undefined : liftM2 (,) (tail x) (tail y))
             (fromPS (op (toPS x) (toPS y)))


liftPowerSum1 :: (Field.C a, ZeroTestable.C a) =>
   ([a] -> [a]) -> ([a] -> [a])
liftPowerSum1 = liftPowerSum1Gen fromPowerSums toPowerSums

liftPowerSum2 :: (Field.C a, ZeroTestable.C a) =>
   ([a] -> [a] -> [a]) -> ([a] -> [a] -> [a])
liftPowerSum2 = liftPowerSum2Gen fromPowerSums toPowerSums

liftPowerSumInt1 :: (Integral.C a, Eq a, ZeroTestable.C a) =>
   ([a] -> [a]) -> ([a] -> [a])
liftPowerSumInt1 = liftPowerSum1Gen PowerSum.toElemSymInt PowerSum.fromElemSym

liftPowerSumInt2 :: (Integral.C a, Eq a, ZeroTestable.C a) =>
   ([a] -> [a] -> [a]) -> ([a] -> [a] -> [a])
liftPowerSumInt2 = liftPowerSum2Gen PowerSum.toElemSymInt PowerSum.fromElemSym




{- * Show -}

appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
  showsPrec p (Cons xs) =
    showParen (p >= appPrec)
       (showString "RootSet.Cons " . shows xs)


{- * Additive -}

{- Use binomial expansion of (x+y)^n -}
add :: (Field.C a, ZeroTestable.C a) => [a] -> [a] -> [a]
add = liftPowerSum2 PowerSum.add

addInt :: (Integral.C a, Eq a, ZeroTestable.C a) => [a] -> [a] -> [a]
addInt = liftPowerSumInt2 PowerSum.add

instance (Field.C a, ZeroTestable.C a) => Additive.C (T a) where
   zero   = const zero
   (+)    = lift2 add
   negate = lift1 PolyCore.alternate


{- * Ring -}

mul :: (Field.C a, ZeroTestable.C a) => [a] -> [a] -> [a]
mul = liftPowerSum2 PowerSum.mul

mulInt :: (Integral.C a, Eq a, ZeroTestable.C a) => [a] -> [a] -> [a]
mulInt = liftPowerSumInt2 PowerSum.mul


pow :: (Field.C a, ZeroTestable.C a) => Integer -> [a] -> [a]
pow n = liftPowerSum1 (PowerSum.pow n)

powInt :: (Integral.C a, Eq a, ZeroTestable.C a) => Integer -> [a] -> [a]
powInt n = liftPowerSumInt1 (PowerSum.pow n)


instance (Field.C a, ZeroTestable.C a) => Ring.C (T a) where
   one           = const one
   fromInteger n = const (fromInteger n)
   (*)           = lift2 mul
   x^n           = lift1 (pow n) x


{- * Field.C -}

instance (Field.C a, ZeroTestable.C a) => Field.C (T a) where
   recip = lift1 reverse


{- * Algebra -}

instance (Field.C a, ZeroTestable.C a) => Algebraic.C (T a) where
   root n = lift1 (PowerSum.root n)



{- |
Given an approximation of a root,
the degree of the polynomial and maximum value of coefficients,
find candidates of polynomials that have approximately this root
and show the actual value of the polynomial at the given root approximation.

This algorithm runs easily into a stack overflow, I do not know why.
We may also employ a more sophisticated integer relation algorithm,
like PSLQ and friends.
-}
{-# SPECIALISE approxPolynomial ::
       Int -> Integer -> Double -> (Double, Poly.T Double) #-}
{-# SPECIALISE approxPolynomial ::
       Int -> Integer -> Float -> (Float, Poly.T Float) #-}
approxPolynomial ::
   (RealRing.C a) =>
   Int -> Integer -> a -> (a, Poly.T a)
approxPolynomial d maxCoeff x =
   let powers = take (d+1) $ iterate (x*) one
   in  -- List.minimumBy (\a b -> compare (abs (fst a)) (abs (fst b))) $
       Key.minimum (abs . fst) $
       map
          ((\cs -> (sum $ zipWith (*) powers cs, Poly.fromCoeffs cs)) . reverse)
          (liftM2 (:)
             (map fromInteger [1 .. maxCoeff])
             (replicateM d $ map fromInteger [-maxCoeff .. maxCoeff]))
