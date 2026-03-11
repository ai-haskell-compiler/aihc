module Numeric.Transform.Fourier.Eigensystem where

import qualified Numeric.Transform.Fourier.FFT as FT
import qualified Data.Complex as Complex
import qualified Data.Array as Array
import Data.Array (Array, Ix, )


(^!) :: Num a => a -> Int -> a
(^!) = (^)

{-# specialize gaussianPlain :: Int -> Float  -> Array Int Float #-}
{-# specialize gaussianPlain :: Int -> Double -> Array Int Double #-}

{- |
For small values of @narrow@ (large width)
it is faster to compute the spectrum of the Gaussian
and apply the Fourier transform on it.
-}
gaussianFourier ::
   (Ix a, Integral a, RealFloat b) =>
   a -- ^ array size
   -> b -- ^ reciprocal of width
   -> Array a b -- ^ X[k]
gaussianFourier n narrow =
   fmap ((/ (narrow*sqrt (fromIntegral n))) . Complex.realPart) $
   FT.rfft $ gaussianPlain n (recip narrow)

gaussianPlain ::
   (Ix a, Integral a, RealFloat b) =>
   a -- ^ array size
   -> b -- ^ reciprocal of width, 1 is the width of the eigenvector
   -> Array a b -- ^ X[k]
gaussianPlain n narrow =
   let nr = fromIntegral n
   in  Array.listArray (0,n-1) $
       map (wrappedGaussian 1e-20 (narrow^!2 * nr * pi)) $
       map (/nr) $
       iterate (1+) 0

{- |
For small @c@ the convergence is slow,
but the result will be close to @recip (sqrt c)@.
-}
wrappedGaussian ::
   (RealFloat b) =>
   b -> b -> b -> b
wrappedGaussian tol c x =
   let xpos = iterate (1+) x
       xneg = tail $ iterate (subtract 1) x
       applyTrunc =
          takeWhile ((>=tol) . abs) .
          map (\xi -> exp (- c*xi*xi))
   in  sum (applyTrunc xpos) +
       sum (applyTrunc xneg)


example0 :: ([Double], [Complex.Complex Double])
example0 =
   let c=0.06; n=13 in (map (\k -> (sqrt (pi/c) *) $ sum $ map (\l -> exp (-pi^!2/(n^!2*c)*(k+n*l)^!2)) [-100..100]) [0..n-1], map (\j -> sum $ map (\k -> exp (((-c*k^!2) Complex.:+ 2*pi*j*k/n))) [-100..100]) [0..n-1])

example1 :: (Double, Double)
example1 =
   let c=0.3 in (sum $ map (\k -> exp(-pi*k^!2/c)) [-100..100], sqrt c * sum (map (\k -> exp(-pi*k^!2*c)) [-100..100]))

exampleAlgebraicDouble :: (Double, Double)
exampleAlgebraicDouble =
   (sum $ map (\k -> exp(-pi*k^!2)) [-100..100], sqrt (4 - sqrt 8) * sum (map (\k -> exp(-pi*2*k^!2)) [-100..100]))

{-
I checked polynomials up to degree 9
and there does not seem to be one,
that has the ratio of the numbers as root.

exampleAlgebraicTriple :: (Double, Double)
exampleAlgebraicTriple =
   (sum $ map (\k -> exp(-pi*k^!2)) [-100..100], sum (map (\k -> exp(-pi*3*k^!2)) [-100..100]))
-}

exampleAlgebraicQuadro :: (Double, Double)
exampleAlgebraicQuadro =
   let a = (2-sqrt(2*sqrt 2)) * (2 + sqrt 2)
       _b = 4 / (2 + sqrt(sqrt 8)) :: Double
   in  (sum $ map (\k -> exp(-pi*k^!2)) [-100..100], a * sum (map (\k -> exp(-pi*4*k^!2)) [-100..100]))

exampleAlgebraicHalf :: (Double, Double)
exampleAlgebraicHalf =
   (sum $ map (\k -> exp(-pi*k^!2)) [-100..100], sqrt (2 - sqrt 2) * sum (map (\k -> exp(-pi/2*k^!2)) [-100..100]))

exampleAlgebraicFourth :: (Double, Double)
exampleAlgebraicFourth =
   let a = (2-sqrt(2*sqrt 2)) * (2+sqrt 2)/2
       _b = 2/(2+sqrt(sqrt 8)) :: Double
   in  (sum $ map (\k -> exp(-pi*k^!2)) [-100..100], a * sum (map (\k -> exp(-pi/4*k^!2)) [-100..100]))

exampleAlgebraic2 :: (Double, Double)
exampleAlgebraic2 =
   let n=2 in (sum $ map (\k -> exp(-pi*n*k^!2)) [-100..100], (1+sqrt 2) * sum (map (\k -> exp(-pi*n*(k+1/n)^!2)) [-100..100]))
{-
a = 1+sqrt 2
a - 1 = sqrt 2
(a - 1)^2 = 2
0 = -1 - 2*a + a^2
-}

exampleAlgebraic3 :: (Double, Double)
exampleAlgebraic3 =
   let n=3 in (sum $ map (\k -> exp(-pi*n*k^!2)) [-100..100], (1+sqrt 3) * sum (map (\k -> exp(-pi*n*(k+1/n)^!2)) [-100..100]))
{-
0 = -2 - 2*a + a^2
-}

exampleAlgebraic4 :: [Double]
exampleAlgebraic4 =
   let n=4 in zipWith (*) [1, 1+sqrt(sqrt 2), (1+sqrt(sqrt 2))/(sqrt(sqrt 2)-1), 1+sqrt(sqrt 2)] $ map (\j -> sum (map (\k -> exp(-pi*n*(k+j/n)^!2)) [-100..100])) [0..n-1]
{-
0 = -1 -  4*a + 6*a^2 -  4*a^3 + a^4
0 =  1 - 12*b + 6*b^2 - 12*b^3 + b^4
-}

exampleAlgebraic5 :: [Double]
exampleAlgebraic5 =
   let n=5; a=1.8743053964841243; b=11.833898536015248 in zipWith (*) [1, a, b, a, 1] $ map (\j -> sum (map (\k -> exp(-pi*n*(k+j/n)^!2)) [-100..100])) [0..n-1]
{-
This must be a linear combination of
(goldenRatio, 1, 0, 0, 1)
(goldenRatio, 0, 1, 1, 0)

with goldenRatio = (1+sqrt 5)/2

0=-4*a^0-4*a^1+26*a^2-14*a^3+a^4
0=-4*b^0-4*b^1+26*b^2-14*b^3+b^4
-}



{-
ToDo:
 - efficient computation of 2-norm of the discrete Gaussian
 - investigate eigenfunctions including Gaussian and Hermite polynomials
 - is there an algebraic ratio between exampleAlgebraicTriple and exampleAlgebraicSixtimes
-}
