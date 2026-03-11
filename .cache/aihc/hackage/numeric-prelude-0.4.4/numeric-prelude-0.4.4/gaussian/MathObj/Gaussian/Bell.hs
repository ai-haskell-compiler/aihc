{-# LANGUAGE RebindableSyntax #-}
{-
Complex translated and modulated Gaussian bell curve.

It could be extended to chirps
using a complex valued quadratic term with (real c >= 0).
This allows for a new test:
Express the Fourier transform in terms of a convolution with a chirp.
-}
module MathObj.Gaussian.Bell where

import qualified MathObj.Polynomial as Poly
import qualified Number.Complex as Complex

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute       as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import Number.Complex ((+:), )

import Test.QuickCheck (Arbitrary, arbitrary, )
import Control.Monad (liftM4, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (reverse, )


{- $setup
>>> import qualified MathObj.Gaussian.Bell as G
>>> import qualified Algebra.ZeroTestable as ZeroTestable
>>> import qualified Algebra.Laws as Laws
>>> import qualified Number.Complex as Complex
>>> import Number.Complex ((+:))
>>> import NumericPrelude.Base as P
>>> import NumericPrelude.Numeric as NP
>>> import Prelude ()
>>> import qualified Test.QuickCheck as QC
>>> import Data.Function.HT (Id, nest)
>>>
>>> asRational :: Id (G.T Rational)
>>> asRational = id
>>>
>>> withRational :: Id (G.T Rational -> a)
>>> withRational = id
>>>
>>> isConstant :: ZeroTestable.C a => G.T a -> Bool
>>> isConstant (G.Cons _amp _a b c) = isZero b && isZero c
-}


data T a = Cons {amp :: a, c0, c1 :: Complex.T a, c2 :: a}
   deriving (Eq, Show)

instance (Absolute.C a, Arbitrary a) => Arbitrary (T a) where
   arbitrary =
      liftM4
         (\k a b c -> Cons (abs k) a b (1 + abs c))
         arbitrary arbitrary arbitrary arbitrary


constant :: Ring.C a => T a
constant = Cons one zero zero zero

{- |
eigenfunction of 'fourier'
-}
unit :: Ring.C a => T a
unit = Cons one zero zero one

{-# INLINE evaluate #-}
evaluate :: (Trans.C a) =>
   T a -> a -> Complex.T a
evaluate f x =
   Complex.scale
     (sqrt (amp f))
     (Complex.exp $ Complex.scale (-pi) $
      c0 f + Complex.scale x (c1 f) + Complex.fromReal (c2 f * x^2))

evaluateSqRt :: (Trans.C a) =>
   T a -> a -> Complex.T a
evaluateSqRt f x0 =
   Complex.scale
     (sqrt (amp f))
     (let x = sqrt pi * x0
      in  Complex.exp $ negate $
          c0 f + Complex.scale x (c1 f) + Complex.fromReal (c2 f * x^2))

exponentPolynomial :: (Additive.C a) =>
   T a -> Poly.T (Complex.T a)
exponentPolynomial f =
   Poly.fromCoeffs [c0 f, c1 f, Complex.fromReal (c2 f)]


{-
norm functions depend on interpretation
and would have to return both a rational and transcendental part
expressed as @exp a@.
-}

variance :: (Trans.C a) =>
   T a -> a
variance f =
   recip $ c2 f * 2*pi

{- |
prop> Laws.identity G.multiply G.constant . asRational
prop> Laws.commutative G.multiply . asRational
prop> Laws.associative G.multiply . asRational
-}
multiply :: (Ring.C a) =>
   T a -> T a -> T a
multiply f g =
   Cons
      (amp f * amp g)
      (c0 f + c0 g) (c1 f + c1 g) (c2 f + c2 g)

powerRing :: (Trans.C a) =>
   Integer -> T a -> T a
powerRing p f =
   let pa = fromInteger p
   in  Cons
          (amp f ^ p)
          (pa * c0 f) (pa * c1 f) (fromInteger p * c2 f)

{-
powerField does not makes sense,
since the reciprocal of a Gaussian diverges.
-}

powerAlgebraic :: (Trans.C a) =>
   Rational -> T a -> T a
powerAlgebraic p f =
   let pa = fromRational' p
   in  Cons
          (amp f ^/ p)
          (pa * c0 f) (pa * c1 f) (fromRational' p * c2 f)

powerTranscendental :: (Trans.C a) =>
   a -> T a -> T a
powerTranscendental p f =
   Cons
      (amp f ^? p)
      (Complex.scale p $ c0 f) (Complex.scale p $ c1 f) (p * c2 f)


{- |
>>> let x=G.Cons 2 (1+:3) (4+:5) (7::Rational); y=G.Cons 7 (1+:4) (3+:2) (5::Rational) in G.convolve x y
Cons {amp = 7 % 6, c0 = 13 % 6 +: 55 % 8, c1 = 41 % 12 +: 13 % 4, c2 = 35 % 12}

prop> Laws.commutative G.convolve . asRational
prop> Laws.associative G.convolve . asRational

Would be nice to have something like:

> Laws.identity G.convolve G.dirac

but we cannot represent @G.dirac@.

prop> isConstant . G.convolve G.constant . asRational

Using a @G.norm1@ we could exactly compute the amplitude
of the resulting constant function.
But that would require transcendent operations.
-}
convolve :: (Field.C a) =>
   T a -> T a -> T a
convolve f g =
   let s = c2 f + c2 g
       {-
       fd = f1/(2*f2)
       gd = g1/(2*g2)
       c = f2*g2/(f2+g2)

       c*(fd+gd) = (f1*g2+f2*g1)/(2*(f2+g2)) = b/2

       c*(fd+gd)^2 - fd^2*f2 - gd^2*g2
         = f2*g2*(fd+gd)^2/(f2 + g2) - (fd^2*f2 + gd^2*g2)
         = (f2*g2*(fd+gd)^2 - (f2+g2)*(fd^2*f2+gd^2*g2)) / (f2 + g2)
         = (2*f2*g2*fd*gd - (fd^2*f2^2+gd^2*g2^2)) / (f2 + g2)
         = (2*f1*g1 - (f1^2+g1^2)) / (4*(f2 + g2))
         = -(f1 - g1)^2/(4*(f2 + g2))
       -}
   in  Cons
          (amp f * amp g / s)
          (c0 f + c0 g
              - Complex.scale (recip (4*s)) ((c1 f - c1 g)^2))
          (Complex.scale (c2 g / s) (c1 f) +
           Complex.scale (c2 f / s) (c1 g))
          (c2 f * c2 g / s)
            -- recip $ recip (c2 f) + recip (c2 g)
{-
   Cons
      (c0 f + c0 g) (c1 f + c1 g)
      (recip $ recip (c2 f) + recip (c2 g))
-}

{- |
prop> withRational $ \x y -> G.convolve x y == G.convolveByTranslation x y
-}
convolveByTranslation :: (Field.C a) =>
   T a -> T a -> T a
convolveByTranslation f0 g0 =
   let fd = Complex.scale (recip (2 * c2 f0)) $ c1 f0
       gd = Complex.scale (recip (2 * c2 g0)) $ c1 g0
       f1 = translateComplex fd f0
       g1 = translateComplex gd g0
       s = c2 f1 + c2 g1
   in  translateComplex (negate $ fd + gd) $
       Cons
          (amp f1 * amp g1 / s)
          (c0 f1 + c0 g1) zero
          (c2 f1 * c2 g1 / s)

{- |
prop> withRational $ \x y -> G.convolve x y == G.convolveByFourier x y
-}
convolveByFourier :: (Field.C a) =>
   T a -> T a -> T a
convolveByFourier f g =
   reverse $ fourier $ multiply (fourier f) (fourier g)

{- |
prop> withRational $ \x y -> G.fourier (G.convolve x y) == G.multiply (G.fourier x) (G.fourier y)
prop> withRational $ \x -> nest 2 G.fourier x == G.reverse x
prop> G.fourier G.unit == (asRational G.unit)
prop> withRational $ \x a -> G.fourier (G.translate a x) == G.modulate a (G.fourier x)
prop> withRational $ \x (QC.Positive a) -> G.fourier (G.dilate a x) == G.amplify a (G.shrink a (G.fourier x))
-}
fourier :: (Field.C a) =>
   T a -> T a
fourier f =
   let a = c0 f
       b = c1 f
       rc = recip $ c2 f
   in  Cons
          (amp f * rc)
          (Complex.scale (rc/4) (-b^2) + a)
          (Complex.scale rc $ Complex.quarterRight b)
          rc

{- |
prop> withRational $ \x -> G.fourier x == G.fourierByTranslation x
-}
fourierByTranslation :: (Field.C a) =>
   T a -> T a
fourierByTranslation f =
   translateComplex (Complex.scale (1/2) $ Complex.quarterLeft $ c1 f) $
   Cons (amp f / c2 f) (c0 f) zero (recip $ c2 f)

{-
a + b*x + c*x^2
 = c*(a/c + b/c*x + x^2)
 = c*((x-b/(2*c))^2 + (4*a*c+b^2)/(4*c^2))
 = c*(x-b/(2*c))^2 + (4*a*c+b^2)/(4*c)

fourier ->
   x^2/c - i*b/c*x + (4*a*c+b^2)/(4*c)

fourier (x -> exp(-pi*c*(x-t)^2))
 = fourier $ translate t $ shrink (sqrt c) $ x -> exp(-pi*x^2)
 = modulate t $ dilate (sqrt c) $ fourier $ x -> exp(-pi*x^2)
 = modulate t $ dilate (sqrt c) $ x -> exp(-pi*x^2)
 = modulate t $ x -> exp(-pi*x^2/c)
 = x -> exp(-pi*x^2/c) * exp(-2*pi*i*x*t)
 = x -> exp(-pi*(x^2/c - 2*i*x*t))
-}

{-
b*x + c*x^2
 = c*(b/c*x + x^2)
 = c*((x-br/(2*c))^2 + i*x*bi/c - br^2/(4*c^2))
 = c*(x-br/(2*c))^2 + i*x*bi - br^2/(4*c)

fourier ->
   (x+bi/2)^2/c - i*br/c*(x+bi/2) - br^2/(4*c)
 = (1/c) * ((x+bi/2)^2 - i*br*(x+bi/2) + (br/2)^2)
 = (1/c) * (x^2 - i*b*x + -(br/2)^2 + (bi/2)^2 - i*br*bi/2)
 = (1/c) * (x^2 - i*b*x - (br^2-bi^2+2*br*bi*i)^2 /4)
 = (1/c) * (x^2 - i*b*x - b^2 / 4)
 = (1/c) * (x^2 - i*b*x + (i*b/2)^2)
 = (1/c) * (x - i*b/2)^2

Example:
  (x-b)^2 = b^2 - 2*b*x + x^2
    ->  (- i*2*b*x + x^2)


fourier (x -> exp(-pi*(c*(x-t)^2 + 2*i*m*x)))
 = fourier $ modulate m $ translate t $ shrink (sqrt c) $ x -> exp(-pi*x^2)
 = translate (-m) $ modulate t $ dilate (sqrt c) $ fourier $ x -> exp(-pi*x^2)
 = translate (-m) $ modulate t $ dilate (sqrt c) $ x -> exp(-pi*x^2)
 = translate (-m) $ modulate t $ x -> exp(-pi*x^2/c)
 = translate (-m) $ x -> exp(-pi*x^2/c) * exp(-2*pi*i*x*t)
 = x -> exp(-pi*(x+m)^2/c) * exp(-2*pi*i*(x+m)*t)
 = x -> exp(-pi*((x+m)^2/c - 2*i*(x+m)*t))
-}

{-
fourier (Cons a 0 0) =
  Cons a 0 infinity

fourier (Cons 0 0 c) =
  Cons 0 0 (recip c)

fourier (Cons 0 b 1) =
  Cons 0 (i*b) 1
-}

{- |
prop> withRational $ \x a b -> G.translate a (G.translate b x) == G.translate (a+b) x
-}
translate :: Ring.C a => a -> T a -> T a
translate d f =
   let a = c0 f
       b = c1 f
       c = c2 f
   in  Cons
          (amp f)
          (Complex.fromReal (c*d^2) - Complex.scale d b + a)
          (Complex.fromReal (-2*c*d) + b)
          c

{- |
prop> withRational $ \x a b -> G.translateComplex a (G.translateComplex b x) == G.translateComplex (a+b) x
prop> withRational $ \x a -> G.translateComplex (Complex.fromReal a) x == G.translate a x
-}
translateComplex :: Ring.C a => Complex.T a -> T a -> T a
translateComplex d f =
   let a = c0 f
       b = c1 f
       c = c2 f
   in  Cons
          (amp f)
          (Complex.scale c (d^2) - b*d + a)
          (Complex.scale (-2*c) d + b)
          c

{- |
prop> withRational $ \x a b -> G.modulate a (G.modulate b x) == G.modulate (a+b) x
prop> withRational $ \x a b -> G.modulate b (G.translate a x) == G.turn (a*b) (G.translate a (G.modulate b x))
-}
modulate :: Ring.C a => a -> T a -> T a
modulate d f =
   Cons
      (amp f)
      (c0 f)
      (c1 f + (zero +: 2*d))
      (c2 f)

turn :: Ring.C a => a -> T a -> T a
turn d f =
   Cons
      (amp f)
      (c0 f + (zero +: 2*d))
      (c1 f)
      (c2 f)

{- |
prop> withRational $ \x -> nest 2 G.reverse x == x
-}
reverse :: Additive.C a => T a -> T a
reverse f =
   f{c1 = negate $ c1 f}


{- |
prop> withRational $ \x (QC.Positive a) (QC.Positive b) -> G.dilate a (G.dilate b x) == G.dilate (a*b) x
prop> withRational $ \x (QC.Positive a) -> G.shrink a x == G.dilate (recip a) x
-}
dilate :: Field.C a => a -> T a -> T a
dilate k f =
   Cons
      (amp f)
      (c0 f)
      (Complex.scale (recip k) $ c1 f)
      (c2 f / k^2)

{- |
prop> withRational $ \x (QC.Positive a) -> G.dilate a (G.shrink a x) == x
prop> withRational $ \x (QC.Positive a) -> G.shrink a (G.dilate a x) == x
-}
shrink :: Ring.C a => a -> T a -> T a
shrink k f =
   Cons
      (amp f)
      (c0 f)
      (Complex.scale k $ c1 f)
      (c2 f * k^2)

amplify :: (Ring.C a) => a -> T a -> T a
amplify k f =
   Cons
      (k^2 * amp f)
      (c0 f)
      (c1 f)
      (c2 f)
