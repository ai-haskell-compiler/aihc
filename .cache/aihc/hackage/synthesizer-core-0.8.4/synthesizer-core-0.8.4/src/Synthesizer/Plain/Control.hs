{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.Plain.Control (
   constant,
   linear,
   linearMultiscale,
   linearMultiscaleNeutral,
   linearStable,
   linearMean,
   line,
   exponential, exponentialMultiscale, exponentialStable,
   exponentialMultiscaleNeutral,
   exponential2, exponential2Multiscale, exponential2Stable,
   exponential2MultiscaleNeutral,
   exponentialFromTo, exponentialFromToMultiscale,
   vectorExponential,
   vectorExponential2,
   cosine, cosineMultiscale, cosineSubdiv, cosineStable,
   cubicHermite,
   cubicHermiteStable,

   -- used in Analysis
   curveMultiscale,
   curveMultiscaleNeutral,
   -- used in Generic.Control, Interpolation.Module
   cubicFunc,
   cosineWithSlope,
   ) where

import qualified Synthesizer.Plain.Signal as Sig

import Data.List (zipWith4, tails, )
import Data.List.HT (iterateAssociative, )

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Field                 as Field
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import Number.Complex (cis,real, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Control curve generation -}

constant :: y -> Sig.T y
constant = repeat


linear :: Additive.C y =>
      y   {-^ steepness -}
   -> y   {-^ initial value -}
   -> Sig.T y {-^ linear progression -}
linear d y0 = iterate (d+) y0

{- |
Minimize rounding errors by reducing number of operations per element
to a logarithmuc number.
-}
linearMultiscale :: Additive.C y =>
      y
   -> y
   -> Sig.T y
linearMultiscale = curveMultiscale (+)

{- |
Linear curve starting at zero.
-}
linearMultiscaleNeutral :: Additive.C y =>
      y
   -> Sig.T y
linearMultiscaleNeutral slope =
   curveMultiscaleNeutral (+) slope zero

{- |
As stable as the addition of time values.
-}
linearStable :: Ring.C y =>
      y
   -> y
   -> Sig.T y
linearStable d y0 =
   curveStable (d*) (+) 1 y0


{- |
It computes the same like 'linear' but in a numerically more stable manner,
namely using a subdivision scheme.
The division needed is a division by two.

> 0       4       8
> 0   2   4   6   8
> 0 1 2 3 4 5 6 7 8
-}
linearMean :: Field.C y =>
      y
   -> y
   -> Sig.T y
linearMean d y0 = y0 :
   foldr (\pow xs -> y0+pow : linearSubdivision xs)
         unreachable (iterate (2*) d)

{- | Intersperse linearly interpolated values. -}
linearSubdivision :: Field.C y =>
      Sig.T y
   -> Sig.T y
linearSubdivision = subdivide (\x0 x1 -> (x0+x1)/2)


{- |
Linear curve of a fixed length.
The final value is not actually reached,
instead we stop one step before.
This way we can concatenate several lines
without duplicate adjacent values.
-}
line :: Field.C y =>
      Int     {-^ length -}
   -> (y,y)   {-^ initial and final value -}
   -> Sig.T y {-^ linear progression -}
line n (y0,y1) =
   take n $ linear ((y1-y0) / fromIntegral n) y0



exponential, exponentialMultiscale, exponentialStable :: Trans.C y =>
      y   {-^ time where the function reaches 1\/e of the initial value -}
   -> y   {-^ initial value -}
   -> Sig.T y {-^ exponential decay -}
exponential time = iterate (* exp (- recip time))
exponentialMultiscale time = curveMultiscale (*) (exp (- recip time))
exponentialStable time = exponentialStableGen exp (- recip time)

exponentialMultiscaleNeutral :: Trans.C y =>
      y   {-^ time where the function reaches 1\/e of the initial value -}
   -> Sig.T y {-^ exponential decay -}
exponentialMultiscaleNeutral time =
   curveMultiscaleNeutral (*) (exp (- recip time)) one

exponential2, exponential2Multiscale, exponential2Stable :: Trans.C y =>
      y   {-^ half life -}
   -> y   {-^ initial value -}
   -> Sig.T y {-^ exponential decay -}
exponential2 halfLife = iterate (*  0.5 ** recip halfLife)
exponential2Multiscale halfLife = curveMultiscale (*) (0.5 ** recip halfLife)
exponential2Stable halfLife = exponentialStableGen (0.5 **) (recip halfLife)

exponential2MultiscaleNeutral :: Trans.C y =>
      y   {-^ half life -}
   -> Sig.T y {-^ exponential decay -}
exponential2MultiscaleNeutral halfLife =
   curveMultiscaleNeutral (*) (0.5 ** recip halfLife) one


exponentialFromTo, exponentialFromToMultiscale :: Trans.C y =>
      y   {-^ time where the function reaches 1\/e of the initial value -}
   -> y   {-^ initial value -}
   -> y   {-^ value after given time -}
   -> Sig.T y {-^ exponential decay -}
exponentialFromTo time y0 y1 =
   iterate (*  (y1/y0) ** recip time) y0
exponentialFromToMultiscale time y0 y1 =
   curveMultiscale (*) ((y1/y0) ** recip time) y0


exponentialStableGen :: (Ring.C y, Ring.C t) =>
      (t -> y)
   -> t
   -> y
   -> Sig.T y
exponentialStableGen expFunc = curveStable expFunc (*)




{-| This is an extension of 'exponential' to vectors
    which is straight-forward but requires more explicit signatures.
    But since it is needed rarely I setup a separate function. -}
vectorExponential :: (Trans.C y, Module.C y v) =>
       y  {-^ time where the function reaches 1\/e of the initial value -}
   ->  v  {-^ initial value -}
   -> Sig.T v {-^ exponential decay -}
vectorExponential time y0 = iterate (exp (-1/time) *>) y0

vectorExponential2 :: (Trans.C y, Module.C y v) =>
       y  {-^ half life -}
   ->  v  {-^ initial value -}
   -> Sig.T v {-^ exponential decay -}
vectorExponential2 halfLife y0 = iterate (0.5**(1/halfLife) *>) y0



cosine, cosineMultiscale, cosineSubdiv, cosineStable :: Trans.C y =>
       y  {-^ time t0 where  1 is approached -}
   ->  y  {-^ time t1 where -1 is approached -}
   -> Sig.T y {-^ a cosine wave where one half wave is between t0 and t1 -}
cosine = cosineWithSlope $
   \d x -> map cos (linear d x)

cosineMultiscale = cosineWithSlope $
   \d x -> map real (curveMultiscale (*) (cis d) (cis x))


{-
  cos (a-b) = cos a * cos b + sin a * sin b
  cos (a+b) = cos a * cos b - sin a * sin b
  cos  a    = (cos (a-b) + cos (a+b)) / (2 * cos b)

  Problem: (cos b) might be close to zero,
  example: Syn.cosineStable 1 (9::Double)
-}
cosineSubdiv =
   let aux d y0 =
          cos y0 :
            foldr (\pow xs -> cos(y0+pow) : cosineSubdivision pow xs)
                  unreachable (iterate (2*) d)
   in  cosineWithSlope aux

cosineSubdivision :: Trans.C y =>
      y
   -> Sig.T y
   -> Sig.T y
cosineSubdivision angle =
   let k = recip (2 * cos angle)
   in  subdivide (\x0 x1 -> (x0+x1)*k)

cosineStable = cosineWithSlope $
   \d x -> map real (exponentialStableGen cis d (cis x))


cosineWithSlope :: Trans.C y =>
      (y -> y -> signal)
   ->  y
   ->  y
   -> signal
cosineWithSlope c t0 t1 =
   let inc = pi/(t1-t0)
   in  c inc (-t0*inc)


cubicHermite :: Field.C y => (y, (y,y)) -> (y, (y,y)) -> Sig.T y
cubicHermite node0 node1 =
   map (cubicFunc node0 node1) (linear 1 0)

{- |
> 0                                     16
> 0               8                     16
> 0       4       8         12          16
> 0   2   4   6   8   10    12    14    16
> 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
-}
cubicFunc :: Field.C y => (y, (y,y)) -> (y, (y,y)) -> y -> y
cubicFunc (t0, (y0,dy0)) (t1, (y1,dy1)) t =
   let dt  = t0-t1
       dt0 = t-t0
       dt1 = t-t1
       x0  = dt1^2
       x1  = dt0^2
   in  ((dy0*dt0 + y0 * (1-2/dt*dt0)) * x0 +
        (dy1*dt1 + y1 * (1+2/dt*dt1)) * x1) / dt^2
{-
cubic t0 (y0,dy0) t1 (y1,dy1) t =
   let x0 = ((t-t1) / (t0-t1))^2
       x1 = ((t-t0) / (t1-t0))^2
   in  y0 * x0 + y1 * x1 +
       (dy0 - y0*2/(t0-t1)) * (t-t0)*x0 +
       (dy1 - y1*2/(t1-t0)) * (t-t1)*x1
-}

cubicHermiteStable :: Field.C y => (y, (y,y)) -> (y, (y,y)) -> Sig.T y
cubicHermiteStable node0 node1 =
   cubicFunc node0 node1 0 :
      foldr (\pow xs ->
                cubicFunc node0 node1 pow : head xs :
                cubicFunc node0 node1 (3*pow) : cubicSubdivision xs)
            unreachable (iterate (2*) 1)

cubicSubdivision :: Field.C y => Sig.T y -> Sig.T y
cubicSubdivision xs =
   let xs0:xs1:xs2:xs3:_ = tails xs
       inter = zipWith4 (\x0 x1 x2 x3 -> (9*(x1+x2) - (x0+x3))/16)
                        xs0 xs1 xs2 xs3
   in  head xs1 : flattenPairs (zip inter xs2)

{-
            foldr (\(pow0:pow1:_) ~(_:xs) ->
                      cos (y0+pow0) : cos (y0+pow1) : cos (y0+pow0+pow1) :
                         cosineSubdivision pow0 xs)
                  unreachable (tails (iterate (2*) d))
-}


{-
maybe cubicHermite could also be implemented in a Multiscale manner
using a difference scheme.

cubicHermiteMultiscale :: Field.C y => (y, (y,y)) -> (y, (y,y)) -> Sig.T y
cubicHermiteMultiscale node0@(t0,y0) node1@(t1,y1) =
   let -- could be inlined and simplified
       ys = map (cubicFunc node0 node1) [0,1,2,3]
       (d0:d1:d2:d3:_) = iterate (mapAdjacent substract) ys

I thought multiplying difference schemes could help somehow,
but it doesn't. :-(

cubicHermiteMultiscale

Leibniz rule for differences

D3(s+r) = D0(s)*D3(r) + 3*D1(s)*D2(r) + 3*D2(s)*D1(r) + D3(s)*D0(r)


mulDiffs4 :: Ring.C a => (a,a,a,a) -> (a,a,a,a) -> (a,a,a,a)
mulDiffs4 (r0,r1,r2,r3) (s0,s1,s2,s3) =
   (r0*s0,
    r0*s1 +   r1*s0,
    r0*s2 + 2*r1*s1 +   r2*s0,
    r0*s3 + 3*r1*s2 + 3*r2*s1 + r3*s0)

mulDiffs4zero :: Ring.C a => (a,a,a) -> (a,a,a) -> (a,a,a)
mulDiffs4zero (r0,r1,r2,r3) (s0,s1,s2,s3) =
   (r0*s0,
    r0*s1 +   r1*s0,
    r0*s2 + 2*r1*s1 +   r2*s0,
    r0*s3 + 3*r1*s2 + 3*r2*s1 + r3*s0)

mulDiffs3 :: Ring.C a => (a,a,a) -> (a,a,a) -> (a,a,a)
mulDiffs3 (r0,r1,r2) (s0,s1,s2) =
   (r0*s0,
    r0*s1 +   r1*s0,
    r0*s2 + 2*r1*s1 +   r2*s0)

mulDiffs3Karatsuba :: Ring.C a => (a,a,a) -> (a,a,a) -> (a,a,a)
mulDiffs3Karatsuba (r0,r1,r2) (s0,s1,s2) =
   let r0s0 = r0*s0
       r1s1 = r1*s1
   in  (r0s0,
        (r0+r1)*(s0+s1) - r0s0 - r1s1,
        r0*s2 + 2*r1s1 + r2*s0)
-}



{- * Auxiliary functions -}

curveStable :: (Additive.C t) =>
      (t -> y)
   -> (y -> y -> y)
   -> t
   -> y
   -> Sig.T y
curveStable expFunc op time y0 =
   y0 : map (op y0)
      (foldr
         (\e xs ->
            let k = expFunc e
            in  k : concatMapPair (\x -> (x, op x k)) xs)
       unreachable (iterate double time))

unreachable :: a
unreachable = error "only reachable in infinity"

double :: Additive.C t => t -> t
double t = t+t

concatMapPair :: (a -> (b,b)) -> Sig.T a -> Sig.T b
concatMapPair f = flattenPairs . map f

flattenPairs :: Sig.T (a,a) -> Sig.T a
flattenPairs = foldr (\(a,b) xs -> a:b:xs) []

subdivide :: (y -> y -> y) -> Sig.T y -> Sig.T y
subdivide f xs0@(x:xs1) =
   x : flattenPairs (zipWith (\x0 x1 -> (f x0 x1, x1)) xs0 xs1)
subdivide _ [] = []


_concatMapPair :: (a -> (b,b)) -> Sig.T a -> Sig.T b
_concatMapPair f = concatMap ((\(x,y) -> [x,y]) . f)


curveMultiscale :: (y -> y -> y) -> y -> y -> Sig.T y
curveMultiscale op d y0 =
   y0 : map (op y0) (iterateAssociative op d)


curveMultiscaleNeutral :: (y -> y -> y) -> y -> y -> Sig.T y
curveMultiscaleNeutral op d neutral =
   neutral : iterateAssociative op d
