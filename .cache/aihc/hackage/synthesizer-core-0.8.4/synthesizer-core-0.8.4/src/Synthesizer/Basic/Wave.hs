{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2006-2010
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Basic waveforms

If you want to use parametrized waves with two parameters
then zip your parameter signals and apply 'uncurry' to the wave function.
-}
module Synthesizer.Basic.Wave (
   T(Cons, decons),
   fromFunction,
   raise,
   amplify,
   distort,
   overtone,
   apply,
   phaseOffset,

   sine,
   cosine,
   helix,
   fastSine2,
   fastSine2Alt,
   fastSine3,
   fastSine3Alt,
   fastSine4,
   fastSine4Alt,
   fastSine4LeastSquares,
   fastSinePolynomials,
   fastSines,
   rationalHelix1,
   rationalHelix1Alt,
   rationalHelix,
   saw,
   sawCos,
   sawComplex,
   superSaw,
   square,
   squareCos,
   squareComplex,
   triangle,
   truncOddCosine,
   truncOddTriangle,
   truncCosine,
   truncTriangle,
   powerNormed,
   powerNormed2,
   logitSaw,
   logitSine,
   sineSquare,
   piecewiseParabolaSaw,
   piecewiseSineSaw,
   sineSawSmooth,
   sineSawSharp,
   sawGaussianHarmonics,
   sawPike,
   trianglePike,
   trianglePikeShift,
   squarePike,
   squarePikeShift,
   squareAsymmetric,
   squareBalanced,
   triangleAsymmetric,
   trapezoid,
   trapezoidAsymmetric,
   trapezoidBalanced,
   trapezoidSkew,

   Harmonic(Harmonic, harmonicPhase, harmonicAmplitude),
   harmonic,
   composedHarmonics,
   ) where

import qualified Synthesizer.Basic.Phase as Phase
import qualified Synthesizer.Basic.Distortion as Distort

import qualified Algebra.RealTranscendental    as RealTrans
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.ZeroTestable          as ZeroTestable
import qualified Algebra.RealField             as RealField
import qualified Algebra.Algebraic             as Algebraic
import qualified Algebra.Module                as Module
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing
import qualified Algebra.Absolute              as Absolute
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive
import qualified Algebra.ToInteger             as ToInteger

import qualified MathObj.Polynomial as Poly
import qualified Number.Complex     as Complex

import qualified Control.Applicative as App

import Data.Bool.HT (select, if', )
import NumericPrelude.Numeric

import NumericPrelude.Base


{- * Definition and construction -}

newtype T t y = Cons {decons :: Phase.T t -> y}


{-# INLINE fromFunction #-}
fromFunction :: (t -> y) -> (T t y)
fromFunction wave = Cons (wave . Phase.toRepresentative)


{- * Operations on waves -}

{-# INLINE raise #-}
raise :: (Additive.C y) => y -> T t y -> T t y
raise y = distort (y+)

{-# INLINE amplify #-}
amplify :: (Ring.C y) => y -> T t y -> T t y
amplify k = distort (k*)

{-# INLINE distort #-}
distort :: (y -> z) -> T t y -> T t z
distort g (Cons f) = Cons (g . f)

{-# INLINE overtone #-}
overtone :: (RealField.C t, ToInteger.C n) => n -> T t y -> T t y
overtone n (Cons f) = Cons (f . Phase.multiply n)

{-# INLINE apply #-}
apply :: T t y -> (Phase.T t -> y)
apply = decons



instance Additive.C y => Additive.C (T t y) where
   {-# INLINE zero #-}
   {-# INLINE (+) #-}
   {-# INLINE (-) #-}
   {-# INLINE negate #-}
   zero = Cons (const zero)
   (+) (Cons f) (Cons g) = Cons (\t -> f t + g t)
   (-) (Cons f) (Cons g) = Cons (\t -> f t - g t)
   negate = distort negate


instance Module.C a y => Module.C a (T t y) where
   {-# INLINE (*>) #-}
   s *> w = distort (s*>) w


instance Functor (T t) where
   {-# INLINE fmap #-}
   fmap = distort

instance App.Applicative (T t) where
   {-# INLINE pure #-}
   pure y = Cons (const y)
   {-# INLINE (<*>) #-}
   Cons f <*> Cons y = Cons (\t -> f t (y t))


{- |
Turn an unparametrized waveform into a parametrized one,
where the parameter is a phase offset.
This way you may express a phase modulated oscillator
using a shape modulated oscillator.

@flip phaseOffset@ could have also be named @rotateLeft@,
since it rotates the wave to the left.
-}
{- disabled SPECIALISE phaseOffset :: (T Double b) -> (Double -> T Double b) -}
{-# INLINE phaseOffset #-}
phaseOffset :: (RealRing.C a) => T a b -> (a -> T a b)
phaseOffset (Cons wave) offset =
   Cons (wave . Phase.increment offset)




{- * Examples -}

{- ** unparameterized -}

{- | map a phase to value of a sine wave -}
{- disabled SPECIALISE sine :: Double -> Double -}
{-# INLINE sine #-}
sine :: Trans.C a => T a a
sine = fromFunction $ \x -> sin (2*pi*x)

{-# INLINE cosine #-}
cosine :: Trans.C a => T a a
cosine = fromFunction $ \x -> cos (2*pi*x)

{-# INLINE helix #-}
helix :: Trans.C a => T a (Complex.T a)
helix = fromFunction $ \x -> Complex.cis (2*pi*x)

{- |
Approximation of sine by parabolas.
Surprisingly it is not really faster than 'sine'.
The wave results from integrating the 'triangle' wave,
thus it the @k@-th harmonic has amplitude @recip (k^3)@.
-}
{-# INLINE fastSine2 #-}
fastSine2 :: (Ord a, Ring.C a) => T a a
fastSine2 = fromFunction $ \x ->
   if 2*x<1
     then -8*x*(2*x-1)
     else 8*(2*x-1)*(x-1)
{-
   if 2*x<1
     then 1 - sqr (4*x-1)
     else sqr (4*x-3) - 1
-}

{-# INLINE fastSine2Alt #-}
fastSine2Alt :: (RealRing.C a) => T a a
fastSine2Alt =
   distort (\x -> 4*x*(1-abs x)) saw

{- |
Piecewise third order polynomial approximation by integrating 'fastSine2'.
-}
{-# INLINE fastSine3 #-}
fastSine3 :: (Ord a, Ring.C a) => T a a
fastSine3 = fromFunction $ \x ->
   if' (4*x<1) (2* x   *(3 - (4* x   )^2)) $
   if' (4*x>3) (2*(x-1)*(3 - (4*(x-1))^2)) $
   (1-2*x) * (3 - (4*x-2)^2)
{-
   if' (4*x<1) ((4*x+1)^2*(1-2*x) - 1) $
   if' (4*x>3) ((4*x-3)^2*(3-2*x) - 1) $
   (1 - 2*(4*x-1)^2*(1-x))
-}

{-# INLINE fastSine3Alt #-}
fastSine3Alt :: (RealRing.C a, Field.C a) => T a a
fastSine3Alt =
   distort (\x -> 0.5*x * (3 - x^2)) triangle

{- |
Piecewise fourth order polynomial approximation by integrating 'fastSine3'.
-}
{-# INLINE fastSine4 #-}
fastSine4 :: (Ord a, Field.C a) => T a a
fastSine4 = fromFunction $ \x ->
   let x2 = 2*x
   in  if x2<1
         then 16/5*x2*(x2-1)*(x2^2-x2-1)
         else 16/5*(2-x2)*(x2-1)*(x2^2-3*x2+1)

{-# INLINE fastSine4Alt #-}
fastSine4Alt :: (RealRing.C a, Field.C a) => T a a
fastSine4Alt =
   distort
      (\x ->
         let ax = 1 - abs x
         in  16/5*ax*x*(1+ax-ax^2))
      saw

{- |
Least squares approximation of sine by fourth order polynomials
computed with MuPad.
-}
{-# INLINE fastSine4LeastSquares #-}
fastSine4LeastSquares :: (Ord a, Trans.C a) => T a a
fastSine4LeastSquares = fromFunction $ \x ->
   -- minimal least squares fit
   let pi2 = pi*pi
       pi3 = pi2*pi
       c = 3*((10080/pi2 - 1050) / pi3 + 1) -- 0.2248391014
       {-# INLINE bow #-}
       bow y = let y2 = y*y in 1-y2*(1+c*(1-y2))
   in  if 2*x<1
         then   bow (4*x-1)
         else - bow (4*x-3)
{-
add a residue to fastSine2 and choose 'c' which minimizes the squared error
   in  if 2*x<1
         then let y = (4*x-1)^2 in 1-y-c*y*(1-y)
         else let y = (4*x-3)^2 in y-1+c*y*(1-y)
-}

{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (0,1::Double)) $ map (\f t -> apply f (Phase.fromRepresentative t)) [sine, fastSine2, fastSine3, fastSine4, fastSine4LeastSquares]
-}

{- |
The coefficient of the highest power is the reciprocal of an element from
<http://oeis.org/A000111>
and the polynomial coefficients are
<http://oeis.org/A119879> .

> mapM_ print $ map (\p -> fmap ((round :: Rational -> Integer) . (/last(Poly.coeffs p))) p) (take 10 $ fastSinePolynomials)
-}
{-# INLINE fastSinePolynomials #-}
fastSinePolynomials :: (Field.C a) => [Poly.T a]
fastSinePolynomials =
   concatMap (\(p0,p1) -> [p0,p1]) $
   iterate
      (\(_,p1) ->
         let integrateNormalize p =
                let pint = Poly.integrate zero p
                in  fmap (/ Poly.evaluate pint one) pint
             p2 = one - integrateNormalize p1
             p3 = integrateNormalize p2
         in  (p2,p3))
      (one, Poly.fromCoeffs [zero, one])
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 100 (-1,1::Double)) (map Poly.evaluate $ take 8 fastSinePolynomials)
-}

{-# INLINE fastSines #-}
fastSines :: (RealField.C a) => [T a a]
fastSines =
   zipWith ($)
      (cycle
         [\p ->
             {- square and (overtone 2 saw) could be generated in one go
                using splitFraction -}
             App.liftA2 (*) square $
                distort (Poly.evaluate p) $
                overtone (2::Int) saw,
          \p -> distort (Poly.evaluate p) triangle])
      fastSinePolynomials
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (-2,2::Double)) $ map (\w t -> apply w (Phase.fromRepresentative t)) (take 10 fastSines)
-}

{- |
This is a helix that is distorted in phase
such that it becomes a purely rational function.
It is guaranteed that the magnitude of the wave is one.
For the distortion factor @recip pi@ you get the closest approximation
to an undistorted helix.
We have chosen this scaling in order to stay with field operations.
-}
{-# INLINE rationalHelix1 #-}
rationalHelix1 :: Field.C a => a -> T a (Complex.T a)
rationalHelix1 k = fromFunction $ \t ->
   let num = k * (2*t-1)
       den = (1-t)*t
   in  negate $
       Complex.scale (recip (den^2+num^2))
          ((den^2-num^2) Complex.+: (2*den*num))
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (0,5::Double)) $ map (. apply (rationalHelix1 (recip pi)) . Phase.fromRepresentative) [Complex.real, Complex.imag]
-}

rationalHelix1Alt :: Field.C a => a -> T a (Complex.T a)
rationalHelix1Alt k = fromFunction $ \t ->
   negate $ helixFromTangent (k * (recip (1-t) - recip t))

{- |
Here we distort the rational helix in phase
using tangent approximations by a sum of 2*n reciprocal functions.
For the tangent function we obtain perfect cosine and sine,
thus for @k = recip pi@ and high @n@ we approach an undistorted complex helix.
-}
{-# INLINE rationalHelix #-}
rationalHelix :: Field.C a => Int -> a -> T a (Complex.T a)
rationalHelix n k = fromFunction $ \t ->
   negate $ helixFromTangent $ (* negate k) $ sum $ take n $
   zipWith (\d0 d1 -> recip (t + d0) + recip (t + d1))
      (tail $ iterate (subtract 1) 0)
      (iterate (1+) 0)

{-# INLINE helixFromTangent #-}
helixFromTangent :: Field.C a => a -> Complex.T a
helixFromTangent t =
   Complex.scale (recip (1+t^2)) ((1-t^2) Complex.+: (2*t))

{- | saw tooth,
it's a ramp down in order to have a positive coefficient for the first partial sine
-}
{- disabled SPECIALISE saw :: Double -> Double -}
{-# INLINE saw #-}
saw :: Ring.C a => T a a
saw = fromFunction $ \x -> 1-2*x

{- |
This wave has the same absolute Fourier coefficients as 'saw'
but the partial waves are shifted by 90 degree.
That is, it is the Hilbert transform of the saw wave.
The formula is derived from 'sawComplex'.
-}
{-# INLINE sawCos #-}
sawCos :: (RealRing.C a, Trans.C a) => T a a
sawCos = fromFunction $ \x -> log (2 * sin (pi*x)) * (-2/pi)

{- |
@sawCos + i*saw@

This is an analytic function and thus it may be used for frequency shifting.

The formula can be derived from the power series of the logarithm function.
-}
{-# INLINE sawComplex #-}
sawComplex ::
   (Complex.Power a, RealTrans.C a, ZeroTestable.C a) =>
   T a (Complex.T a)
sawComplex = fromFunction $ \x -> log (1 + Complex.cis (-pi*(1-2*x))) * (-2/pi)
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 100 (0,1::Double)) [Complex.real . apply sawComplex . Phase.fromRepresentative, apply sawCos . Phase.fromRepresentative]

GNUPlot.plotFuncs [] (GNUPlot.linearScale 100 (0,1::Double)) [sawCos, composedHarmonics (take 20 $ harmonic 0 0 : map (\n -> harmonic 0.25 ((2/pi) / fromInteger n)) [1..])]
-}

{-
Matching implementation that do not match 'saw' exactly.

sawCos :: (RealRing.C a, Trans.C a) => T a a
sawCos = fromFunction $ \x -> log (2 * abs (cos (pi*x)))

sawComplex ::
   (Complex.Power a, Trans.C a) =>
   T a (Complex.T a)
sawComplex = fromFunction $ \x -> log (1 + Complex.cis (2*pi*x))
-}


{-
sum [0..n-1] $ \k -> fraction (x+k*d)
sum [0..n-1] $ \k -> fraction (x+k*(floor d + fraction d))
sum [0..n-1] $ \k -> fraction (x+k*floor d + k*fraction d)
sum [0..n-1] $ \k -> fraction (x + k*fraction d)
sum [0..n-1] $ \k -> fraction (fraction x + k*fraction d)
X = fraction x (0<=X<1)
D = fraction d (0<=D<1)
sum [0..n-1] $ \k -> X+k*D - floor (X+k*D)
n*X + n*(n-1)/2*D - sum [0..n-1] (\k -> floor (X+k*D))

n=1: sum = 0
n=2: sum = floor(X+D)
n=3: sum = floor(X+D)(<2) + floor(X+2*D)(<3)
n=4: sum = floor(X+D) + floor(X+2*D) + floor(X+3*D)

N=n-1
special case: N*D<1   => X+N*D<2

X+(k-1)*D < 1
k-1 < (1-X)/D
k = ceiling((1-X)/D)

sum: max 0 (n - ceiling((1-X)/D))
   = n - min n (ceiling((1-X)/D))

https://math.stackexchange.com/questions/116689/counting-integral-lattice-points-in-a-triangle-that-may-not-have-integer-coordin
-}
{- |
@superSaw n d@ requires @1 <= n@ and @n*d <= 1@.
-}
superSaw, _superSaw :: (RealRing.C a, Field.C a) => Int -> a -> T a a
superSaw n =
   let nf = fromIntegral n
   in \d ->
      fmap (\y -> 1-2*y) $
      fromFunction $ \x ->
         (x + (nf-1)/2*d - 1) + min 1 (ceiling((1-x)/d) / nf)

_superSaw n d =
   fmap (\y -> 1 - 2/fromIntegral n * y) $
   fromFunction $ \x ->
      sum $ map fraction $ take n $ iterate (d+) x


{- | square -}
{- disabled SPECIALISE square :: Double -> Double -}
{-# INLINE square #-}
square :: (Ord a, Ring.C a) => T a a
square = fromFunction $ \x -> if 2*x<1 then 1 else -1

{- |
This wave has the same absolute Fourier coefficients as 'square'
but the partial waves are shifted by 90 degree.
That is, it is the Hilbert transform of the saw wave.
-}
{-# INLINE squareCos #-}
squareCos :: (RealField.C a, Trans.C a) => T a a
squareCos = fromFunction $ \x ->
   log (abs (tan (pi*x))) * (-2/pi)
   -- sawCos x - sawCos (fraction (0.5-x))

{- |
@squareCos + i*square@

This is an analytic function and thus it may be used for frequency shifting.

The formula can be derived from the power series of the area tangens function.
-}
{-# INLINE squareComplex #-}
squareComplex ::
   (Complex.Power a, RealTrans.C a, ZeroTestable.C a) =>
   T a (Complex.T a)
squareComplex = fromFunction $ \x ->
{- these formulas are equivalent but wrong

   log (0 +: 2 * sine x) * (2/pi)

   log ((1 - Complex.cis (-2*pi*x)) *
        (1 + Complex.cis ( 2*pi*x))) * (2/pi)

   sawComplex x + sawComplex (0.5-x)
-}

{-
The Fourier series is equal to the power series of 'atanh'.
-}
   atanh (Complex.cis (2*pi*x)) * (4/pi)
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 100 (0,1::Double)) [squareCos, composedHarmonics (take 20 $ zipWith (\b n -> harmonic 0.25 (if b then (4/pi) / fromInteger n else 0)) (cycle [False,True]) [0..])]
-}

{-
The harmonics 0,0,1,0,-1,0,1,0,-1 etc.
would lead to tangent function wave.
This can be derived from dividing
the series for sine by the series for cosine:
   [-1, _0_, 1] / [1, _0_, 1]
-}


{- | triangle -}
{- disabled SPECIALISE triangle :: Double -> Double -}
{-# INLINE triangle #-}
triangle :: (Ord a, Ring.C a) => T a a
triangle = fromFunction $ \x ->
   let x4 = 4*x
   in  select (2-x4)
          [(x4<1, x4),
           (x4>3, x4-4)]

{-

int(arctan(x)/x,x);

- polylog(2, x*I)*1/2*I + polylog(2, x*(-I))*1/2*I


series(int(arctan(x)/x,x),x,10);

x - 1/9*x^3 + 1/25*x^5 - 1/49*x^7 + 1/81*x^9 + O(x^11)



int(arctan(I*x)/(I*x),x);
int(arctanh(x)/(x),x);

1/2*polylog(2, x) - 1/2*polylog(2, -x)
int(1/x*arctanh(x), x)

polylog(2,x) = dilog(1-x);    -- dilog is implemented in GSL for complex arguments
polylog(2,x) = hypergeom([1,1,1],[2,2],x) * x;


series(int(arctan(I*x)/(I*x),x),x,10);

x + 1/9*x^3 + 1/25*x^5 + 1/49*x^7 + 1/81*x^9 + O(x^11)
-}


{- ** discretely parameterized -}

{- |
A truncated cosine. This has rich overtones.
-}
truncOddCosine :: Trans.C a =>
   Int -> T a a
truncOddCosine k =
   let f = pi * fromIntegral (2*k+1)
   in  fromFunction $ \ x -> cos (f*x)

{- |
For parameter zero this is 'saw'.
-}
truncOddTriangle :: (RealField.C a) =>
   Int -> T a a
truncOddTriangle k =
   let s = fromIntegral (2*k+1)
   in  fromFunction $ \ x ->
          let (n,frac) = splitFraction (s*x)
          in  if even (n::Int)
                then 1-2*frac
                else 2*frac-1


{- ** continuously parameterized -}

{- |
A truncated cosine plus a ramp that guarantees a bump of high 2 at the boundaries.

It is @truncCosine (2 * fromIntegral n + 0.5) == truncOddCosine (2*n)@
-}
truncCosine :: Trans.C a =>
   a -> T a a
truncCosine k =
   let f = 2 * pi * k
       s = 2 * (sin (f*0.5) - 1)
   in  fromFunction $ \ x0 ->
          let x = x0-0.5
          in  - sin (f*x) + s*x
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (0,1::Double)) (map truncCosine [0.5,0.7..2.5])
-}

truncTriangle :: (RealField.C a) =>
   a -> T a a
truncTriangle k =
   let tr x =
          let (n,frac) = splitFraction (2*k*x+0.5)
          in  if even (n::Int)
                then 1-2*frac
                else 2*frac-1
       s = 2 * (1 + tr 0.5)
   in  fromFunction $ \ x0 ->
          let x = x0-0.5
          in  tr x - s*x
{-
GNUPlot.plotFuncs [] (GNUPlot.linearScale 1000 (0,1::Double)) (map truncTriangle [0,0.25..2.5])
-}


{- |
Power function.
Roughly the map @\p x -> x**p@
but retains the sign of @x@ and
it should normalize the mapping over @[-1,1]@ to an L2 norm of 1,
but I got this one wrong.

The sign is flipped with respect to 'saw' and 'sine'
which is an historical artifact.
-}
{-# DEPRECATED powerNormed "Use powerNormed2 instead." #-}
{-# INLINE powerNormed #-}
powerNormed :: (Absolute.C a, Trans.C a) => a -> T a a
powerNormed p = amplify (-p-0.5) $ distort (Distort.powerSigned p) saw

{- |
Power function.
Roughly the map @\p x -> x**p@
but retains the sign of @x@ and
normalizes the mapping over @[0,1]@ to an L2 norm of 1.
-}
{-# INLINE powerNormed2 #-}
powerNormed2 :: (Absolute.C a, Trans.C a) => a -> T a a
powerNormed2 p = amplify (sqrt (1+2*p)) $ distort (Distort.powerSigned p) saw


{- |
Tangens hyperbolicus allows interpolation
between some kind of saw tooth and square wave.
In principle it is not necessary
because you can distort a saw tooth oscillation by @map tanh@.
-}
logitSaw :: (Trans.C a) => a -> T a a
logitSaw c = distort tanh $ amplify c saw


{- |
Tangens hyperbolicus of a sine allows interpolation
between some kind of sine and square wave.
In principle it is not necessary
because you can distort a square oscillation by @map tanh@.
-}
logitSine :: (Trans.C a) => a -> T a a
logitSine c = distort tanh $ amplify c sine


{- |
Interpolation between 'sine' and 'square'.
-}
{-# INLINE sineSquare #-}
sineSquare :: (RealRing.C a, Trans.C a) =>
      a {- ^ 0 for 'sine', 1 for 'square' -}
   -> T a a
sineSquare c =
   distort (Distort.powerSigned (1-c)) sine



{- |
Interpolation between 'fastSine2' and 'saw'.
We just shrink the parabola towards the borders
and insert a linear curve such that its slope matches the one of the parabola.
-}
{-# INLINE piecewiseParabolaSaw #-}
piecewiseParabolaSaw :: (Algebraic.C a, Ord a) =>
      a {- ^ 0 for 'fastSine2', 1 for 'saw' -}
   -> T a a
piecewiseParabolaSaw c =
   let xb  = (1 - sqrt c) / 2
       y x = 1 - ((4*x - (1-c))/(1-c))^2
   in  fromFunction $ \ x ->
       select
          ((2*x - 1)/(2*xb - 1) * y xb)
          [(x <   xb,   y x),
           (x > 1-xb, - y (1-x))]

{-
equ0 c x =
   let y  = 1 - ((4*x - (3+c))/(1-c))^2
       secant  = y/(x-1/2)
       tangent = - 8 * (4*x - (3+c))/(1-c)^2
   in  (tangent, secant)

equ1 c x =
   let secant  = (1 - ((4*x - (3+c))/(1-c))^2)/(x-1/2)
       tangent = - 8 * (4*x - (3+c))/(1-c)^2
   in  (tangent, secant)

equ2 c x =
   (1, ((4*x - (3+c))/(1-c))^2
              - 8 * (x-1/2) * (4*x - (3+c))/(1-c)^2)

equ3 c x =
   ((1-c)^2,
        (4*x - (3+c) - 4 * (2*x-1)) * (4*x - (3+c)))

equ4 c x =
   (4*x - (1-c)) * (4*x - (3+c)) + (1-c)^2

equ5 c x =
   (4*x - 2) ^ 2 - (1+c)^2 + (1-c)^2

equ6 c x =
   (4*x - 2) ^ 2 - 4*c
-}


{- |
Interpolation between 'sine' and 'saw'.
We just shrink the sine towards the borders
and insert a linear curve such that its slope matches the one of the sine.
-}
{-# INLINE piecewiseSineSaw #-}
piecewiseSineSaw :: (Trans.C a, Ord a) =>
      a {- ^ 0 for 'sine', 1 for 'saw' -}
   -> T a a
piecewiseSineSaw c =
   let {- This simple fix point iteration converges very slow for small 'c',
          maybe we should use a Newton iteration. -}
       iter z = iterate (\zi -> pi + atan (zi - pi / (1-c))) z !! 10
       xb = (1-c)/(2*pi) * iter 0
       -- iter (xInit * (2*pi) / (1-c))
       -- xb  = (1 - sqrt c) / 2
       -- y x = sine (x/(1-c))
       y x = sin (2*pi*x/(1-c))
   in  fromFunction $ \ x -> select
          ((2*x - 1)/(2*xb - 1) * y xb)
          [(x <   xb,   y x),
           (x > 1-xb, - y (1-x))]

{-
equ0 c x =
   let secant  = 2 * sin (2*pi*x/(1-c)) / (2*x - 1)
       tangent = 2*pi/(1-c) * cos (2*pi*x/(1-c))
   in  (tangent, secant)

iter0 c x =
   -- secant / tangent
   -- (x - 1/2) = tan (2*pi*x/(1-c)) * (1-c) / (2*pi)
   tan (2*pi*x/(1-c)) * (1-c) / (2*pi) + 1/2

iter1 c x =
   (1-c)/(2*pi) * (pi + atan ((x - 1/2) * (2*pi) / (1-c)))

iter2 c x =
   let iter z = iterate (\zi -> pi + atan (zi - pi / (1-c))) z !! 10
   in  (1-c)/(2*pi) * iter (x * (2*pi) / (1-c))
-}


{- |
Interpolation between 'sine' and 'saw'
with smooth intermediate shapes but no perfect saw.
-}
{-# INLINE sineSawSmooth #-}
sineSawSmooth :: (Trans.C a) =>
      a {- ^ 0 for 'sine', 1 for 'saw' -}
   -> T a a
sineSawSmooth c =
   distort (\x -> sin (affineComb c (pi * x, asin x * 2))) saw

{- |
Interpolation between 'sine' and 'saw'
with perfect saw, but sharp intermediate shapes.
-}
{-# INLINE sineSawSharp #-}
sineSawSharp :: (Trans.C a) =>
      a {- ^ 0 for 'sine', 1 for 'saw' -}
   -> T a a
sineSawSharp c =
   distort (\x -> sin (affineComb c (pi * x, asin x))) saw


affineComb :: Ring.C a => a -> (a,a) -> a
affineComb phase (x0,x1) = (1-phase)*x0 + phase*x1


{- |
Harmonics of a saw wave that is smoothed by a Gaussian lowpass filter.
This can also be used to interpolate between saw wave and sine.
The parameter is the cutoff-frequency
defined as the standard deviation of the Gaussian in frequency space.
That is, high values approximate a saw and need many harmonics,
whereas low values tend to a sine and need only few harmonics.
-}
sawGaussianHarmonics ::
   (RealField.C a, Trans.C a) => a -> [Harmonic a]
sawGaussianHarmonics cutoff =
   (harmonic zero 0 :) $
   map (\n -> harmonic zero (exp (-(n/cutoff)^2 / 2) * 2 / (pi*n))) $
   iterate (1+) 1

{-
{- |
Smooth saw generated by a quintic polynomial function.
Unfortunately if 'c' approaches the right border,
the function will overshoot the 'y' range (-1,1).
-}
quinticSaw :: Field.C a =>
      a  {- ^ position of the right minimum -}
   -> a
   -> a
quinticSaw c x =
   let (s,t) = ToneMod.solveSLE2 ((c^2-1, 3*c^2-1), (c^4-1, 5*c^4-1)) (-1/c,0)
       r = - s - t
       x2 = x^2
   in  x * (r + x2 * (s + x2*t))
{-
       r*x + s*  x^3 + t*  x^5
   0 = r   + s       + t
  -1 = r*c + s*  c^3 + t*  c^5
   0 = r   + s*3*c^2 + t*5*c^4

-1/c = r   + s*  c^2 + t*  c^4

-1/c = s*(c^2-1)   + t*(c^4-1)
   0 = s*(3*c^2-1) + t*(5*c^4-1)
-}
-}


{- |
saw with space
-}
{- disabled SPECIALISE sawPike :: Double -> Double -> Double -}
{-# INLINE sawPike #-}
sawPike :: (Ord a, Field.C a) =>
      a {- ^ pike width ranging from 0 to 1, 1 yields 'saw' -}
   -> T a a
sawPike r = fromFunction $ \x ->
   if x<r
     then 1-2/r*x
     else 0

{- |
triangle with space
-}
{- disabled SPECIALISE trianglePike :: Double -> Double -> Double -}
{-# INLINE trianglePike #-}
trianglePike :: (RealRing.C a, Field.C a) =>
      a  {- ^ pike width ranging from 0 to 1, 1 yields 'triangle' -}
   -> T a a
trianglePike r = fromFunction $ \x ->
   if x < 1/2
     then max 0 (1 - abs (4*x-1) / r)
     else min 0 (abs (4*x-3) / r - 1)

{- |
triangle with space and shift
-}
{- disabled SPECIALISE trianglePikeShift :: Double -> Double -> Double -> Double -}
{-# INLINE trianglePikeShift #-}
trianglePikeShift :: (RealRing.C a, Field.C a) =>
      a  {- ^ pike width ranging from 0 to 1 -}
   -> a  {- ^ shift ranges from -1 to 1; 0 yields 'trianglePike' -}
   -> T a a
trianglePikeShift r s = fromFunction $ \x ->
   if x < 1/2
     then max 0 (1 - abs (4*x-1+s*(r-1)) / r)
     else min 0 (abs (4*x-3+s*(1-r)) / r - 1)

{- |
square with space,
can also be generated by mixing square waves with different phases
-}
{- disabled SPECIALISE squarePike :: Double -> Double -> Double -}
{-# INLINE squarePike #-}
squarePike :: (RealRing.C a) =>
      a  {- ^ pike width ranging from 0 to 1, 1 yields 'square' -}
   -> T a a
squarePike r = fromFunction $ \x ->
   if 2*x < 1
     then if abs(4*x-1)<r then  1 else 0
     else if abs(4*x-3)<r then -1 else 0

{- |
square with space and shift
-}
{- disabled SPECIALISE squarePikeShift :: Double -> Double -> Double -> Double -}
{-# INLINE squarePikeShift #-}
squarePikeShift :: (RealRing.C a) =>
      a  {- ^ pike width ranging from 0 to 1 -}
   -> a  {- ^ shift ranges from -1 to 1; 0 yields 'squarePike' -}
   -> T a a
squarePikeShift r s = fromFunction $ \x ->
   if 2*x < 1
     then if abs(4*x-1+s*(r-1))<r then  1 else 0
     else if abs(4*x-3+s*(1-r))<r then -1 else 0


{- |
square with different times for high and low
-}
{- disabled SPECIALISE squareAsymmetric :: Double -> Double -> Double -}
{-# INLINE squareAsymmetric #-}
squareAsymmetric :: (Ord a, Ring.C a) =>
      a  {- ^ value between -1 and 1 controlling the ratio of high and low time:
              -1 turns the high time to zero,
               1 makes the low time zero,
               0 yields 'square' -}
   -> T a a
squareAsymmetric r = fromFunction $ \x ->
   if 2*x < r+1 then 1 else -1

{- | Like 'squareAsymmetric' but with zero average.
It could be simulated by adding two saw oscillations
with 180 degree phase difference and opposite sign.
-}
{- disabled SPECIALISE squareBalanced :: Double -> Double -> Double -}
{-# INLINE squareBalanced #-}
squareBalanced :: (Ord a, Ring.C a) => a -> T a a
squareBalanced r =
   raise (-r) $ squareAsymmetric r

{- |
triangle
-}
{- disabled SPECIALISE sawPike :: Double -> Double -> Double -}
{-# INLINE triangleAsymmetric #-}
triangleAsymmetric :: (Ord a, Field.C a) =>
      a  {- ^ asymmetry parameter ranging from -1 to 1:
              For 0 you obtain the usual triangle.
              For -1 you obtain a falling saw tooth starting with its maximum.
              For 1 you obtain a rising saw tooth starting with a zero. -}
   -> T a a
triangleAsymmetric r = fromFunction $ \x ->
   select ((2-4*x)/(1-r))
      [(4*x < 1+r, 4/(1+r)*x),
       (4*x > 3-r, 4/(1+r)*(x-1))]

{- |
Mixing 'trapezoid' and 'trianglePike' you can get back a triangle wave form
-}
{- disabled SPECIALISE trapezoid :: Double -> Double -> Double -}
{-# INLINE trapezoid #-}
trapezoid :: (RealRing.C a, Field.C a) =>
      a  {- ^ width of the plateau ranging from 0 to 1:
              0 yields 'triangle', 1 yields 'square' -}
   -> T a a
trapezoid w = fromFunction $ \x ->
   if x < 1/2
     then min   1  ((1 - abs (4*x-1)) / (1-w))
     else max (-1) ((abs (4*x-3) - 1) / (1-w))

{- |
Trapezoid with distinct high and low time.
That is the high and low trapezoids are symmetric itself,
but the whole waveform is not symmetric.
-}
{- disabled SPECIALISE trapezoidAsymmetric :: Double -> Double -> Double -> Double -}
{-# INLINE trapezoidAsymmetric #-}
trapezoidAsymmetric :: (RealRing.C a, Field.C a) =>
      a  {- ^ sum of the plateau widths ranging from 0 to 1:
              0 yields 'triangleAsymmetric',
              1 yields 'squareAsymmetric' -}
   -> a  {- ^ asymmetry of the plateau widths ranging from -1 to 1 -}
   -> T a a
trapezoidAsymmetric w r = fromFunction $ \x ->
   let c0 = 1+w*r
       c1 = 1-w*r
   in  if 2*x < c0
         then min   1  ((c0 - abs (4*x-c0)) / (1-w))
         else max (-1) ((abs (4*(1-x)-c1) - c1) / (1-w))
{-
   let c = w*r+1
   in  if 2*x < c
         then min   1  ((1 - abs (4*x/c-1))*c/(1-w))
         else max (-1) ((abs (4*(1-x)/(2-c)-1) - 1)*(2-c)/(1-w))
-}
{-
   let c = (w*r+1)/2
   in  if x < c
         then min   1  ((1 - abs (2*x/c-1))*2*c/(1-w))
         else max (-1) ((abs (2*(1-x)/(1-c)-1) - 1)*2*(1-c)/(1-w))
-}

{- |
trapezoid with distinct high and low time and zero direct current offset
-}
{- disabled SPECIALISE trapezoidBalanced :: Double -> Double -> Double -> Double -}
{-# INLINE trapezoidBalanced #-}
trapezoidBalanced :: (RealRing.C a, Field.C a) => a -> a -> T a a
trapezoidBalanced w r =
   raise (-w*r) $ trapezoidAsymmetric w r


-- could also be generated by amplifying and clipping a saw ramp
{- |
parametrized trapezoid that can range from a saw ramp to a square waveform.
-}
trapezoidSkew :: (Ord a, Field.C a) =>
      a   {- ^ width of the ramp,
               that is 1 yields a downwards saw ramp
               and 0 yields a square wave. -}
   -> T a a
trapezoidSkew w =
   fromFunction $ \t ->
   if' (2*t<=1-w)   1  $
   if' (2*t>=1+w) (-1) $
   (1-2*t)/w

{- |
This is similar to Polar coordinates,
but the range of the phase is from @0@ to @1@, not @0@ to @2*pi@.

If you need to represent a harmonic by complex coefficients
instead of the polar representation,
then please build a complex valued polynomial from your coefficients
and use it to distort a 'helix'.

> distort (Poly.evaluate (Poly.fromCoeffs complexCoefficients)) helix
-}
data Harmonic a =
   Harmonic {harmonicPhase :: Phase.T a, harmonicAmplitude :: a}

{-# INLINE harmonic #-}
harmonic :: Phase.T a -> a -> Harmonic a
harmonic = Harmonic

{- |
Specify the wave by its harmonics.

The function is implemented quite efficiently
by applying the Horner scheme to a polynomial with complex coefficients
(the harmonic parameters)
using a complex exponential as argument.
-}
{-# INLINE composedHarmonics #-}
composedHarmonics :: Trans.C a => [Harmonic a] -> T a a
composedHarmonics hs =
   let p = Poly.fromCoeffs $
              map (\h -> Complex.fromPolar (harmonicAmplitude h)
                      (2*pi * Phase.toRepresentative (harmonicPhase h))) hs
   in  distort (Complex.imag . Poly.evaluate p) helix
{-
GNUPlot.plotFunc [] (GNUPlot.linearScale 1000 (0,1::Double)) (composedHarmonics [harmonic 0 0, harmonic 0 0, harmonic 0 0, harmonic 0.25 1])
-}
