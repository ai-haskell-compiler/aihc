{-# LANGUAGE NoImplicitPrelude #-}
{- |
The distortion functions have slope 1 at zero,
if they are differentiable at that point, at all.
This ensures that signals with low amplitude
are only slightly altered.
Non-differentiable distortions try to have an overall slope of 1.
-}
module Synthesizer.Basic.Distortion (
   clip, logit,
   zigZag, sine,
   oddChebyshev, {- swing, -}
   quantize,
   powerSigned,
   ) where

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Absolute              as Absolute
import qualified Algebra.Ring                  as Ring

import Data.List.HT (mapAdjacent, )
import Data.Ord.HT (limit, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Clipping -}

{- |
limit, fuzz booster
-}
clip :: (RealRing.C a) => a -> a
clip = limit (negate one, one)

{- |
logit, tanh
-}
logit :: (Trans.C a) => a -> a
logit = tanh

{-
probit, error function
-}



{- * Wrapping -}

{- |
zig-zag
-}
zigZag :: (RealField.C a) => a -> a
zigZag x =
   let (n,y) = splitFraction ((x+1)/2)
   in  if even (n::Int)
         then 2*y - 1
         else 1 - 2*y

{- |
sine
-}
sine :: (Trans.C a) => a -> a
sine = sin


{- |
Odd Chebyshev polynomial

@oddChebyshev n@ is an appropriately scaled Chebyshev polynomial of order @2*n+1@.
The argument @n@ must be non-negative.

> Graphics.Gnuplot.Simple.plotFuncs [Graphics.Gnuplot.Simple.YRange (-1,1)] (Graphics.Gnuplot.Simple.linearScale 1000 (-7,7::Double)) (List.map oddChebyshev [0..5])
-}
oddChebyshev :: (Trans.C a) => (Field.C a) => Int -> a -> a
oddChebyshev n xn =
   let order = 2*n+1
       {-
       slope of normal Chebyshev polynomials at zero is @order@
       which can be seen when considering slope of @x -> cos (order * arccos x)@
       -}
       x  = parityFlip n (xn / fromIntegral order)
       ys = 1 : x : mapAdjacent (\x0 x1 -> 2*x*x1 - x0) ys
   in  ys !! order

parityFlip :: Ring.C a => Int -> a -> a
parityFlip n x =
   if even n then x else -x


{- |
A polynomial function with zeros at every integral point
weighted in order to equalize the local extreme points.

However, the weighting is difficult enough,
that it might be easier to use just a truncated Taylor series of sine.

We could compute a weighting denominator polynomial
by dividing our equidistant zeros polynomial by the sine series.

equidist / weight = sine
weight = equidist / sine

However we have to normalize the zeros,
thus powers of pi enter the scene
and then power series division becomes inexact.
-}
_swing :: (Trans.C a) => (Field.C a) => Int -> a -> a
_swing n x =
{-
   foldl (*) x
      (map
         (\ni ->
            let x2 = x^2
                n2 = ni^2
            in  (x2-n2)/sqrt(x2+n2))
         (take n (iterate (1+) 1)))
-}
   foldl (*) x
      (map
         (\ni ->
            let x2 = x^2
                n2 = ni^2
            in  (x2-n2)/(x2+n2))
         (take n (iterate (1+) 1)))
{-
   foldl (*) x
      (map (\ni -> (x/ni)^2-1)
         (take n (iterate (1+) 1)))
-}
{-
   let xu = iterate (1+) x
       xl = iterate (subtract 1) x
   in  foldl (*) x (take n (tail (zipWith (*) xu xl)))
-}
--   in  product (x : take n (tail xu) ++ take n (tail xl))



{- * Quantization -}

quantize :: (RealField.C a) => a -> a
quantize x = fromIntegral (round x :: Int)


{- * other -}

{- |
Power function.
Roughly the map @\p x -> x**p@ but retains the sign of @x@.
-}
{-# INLINE powerSigned #-}
powerSigned :: (Absolute.C a, Trans.C a) => a -> a -> a
powerSigned p x = signum x * abs x ** p
