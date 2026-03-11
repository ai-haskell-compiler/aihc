{-# LANGUAGE RebindableSyntax #-}
{-
We represent a Gaussian bell curve in terms of the reciprocal of its variance
and its value at the origin.

We could do some projective geometry in the exponent
in order to also have zero variance,
which corresponds to the dirac impulse.

The Gaussians form a nice multiplicative commutative monoid.
Maybe we should have such a structure.
It would also be useful for the Root data type
and a new Exponential data type.
-}
module MathObj.Gaussian.Variance where

import qualified MathObj.Polynomial as Poly
import qualified Number.Root as Root

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Algebraic      as Algebraic
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute       as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import Test.QuickCheck (Arbitrary, arbitrary, )
import Control.Monad (liftM2, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{- $setup
>>> import qualified MathObj.Gaussian.Variance as G
>>> import MathObj.Gaussian.ExponentTuple (HoelderConjugates(HoelderConjugates))
>>> import MathObj.Gaussian.ExponentTuple (YoungConjugates(YoungConjugates))
>>> import qualified Algebra.Laws as Laws
>>> import qualified Number.Root as Root
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
-}


{- |
Since @amp@ is the square of the actual amplitude it must be non-negative.
-}
data T a = Cons {amp, c :: a}
   deriving (Eq, Show)

instance (Absolute.C a, Arbitrary a) => Arbitrary (T a) where
   arbitrary =
      liftM2 Cons
         (fmap abs arbitrary)
         (fmap ((1+) . abs) arbitrary)


constant :: Ring.C a => T a
constant = Cons one zero

{- |
eigenfunction of 'fourier'
-}
unit :: Ring.C a => T a
unit = Cons one one

{-# INLINE evaluate #-}
evaluate :: (Trans.C a) =>
   T a -> a -> a
evaluate f x =
   sqrt (amp f) * exp (-pi * c f * x^2)

exponentPolynomial :: (Additive.C a) =>
   T a -> Poly.T a
exponentPolynomial f =
   Poly.fromCoeffs [zero, zero, c f]


integrateRoot :: (Field.C a) => T a -> Root.T a
integrateRoot f =
   Root.sqrt $ Root.fromNumber $ amp f / c f

{- |
Cauchy-Schwarz inequality:

prop> withRational $ \x y -> G.scalarProductRoot x y <= G.norm2Root x `Root.mul` G.norm2Root y

Hoelder inequality:

prop> withRational $ \x y -> G.scalarProductRoot x y <= G.norm1Root x `Root.mul` G.normInfRoot y
prop> withRational $ \x y (HoelderConjugates p q) -> G.scalarProductRoot x y <= G.normPRoot p x `Root.mul` G.normPRoot q y
-}
scalarProductRoot :: (Field.C a) => T a -> T a -> Root.T a
scalarProductRoot f g =
   integrateRoot (multiply f g)


{- |
prop> withRational $ \x -> G.norm1Root x == G.normPRoot 1 x
-}
norm1Root :: (Field.C a) => T a -> Root.T a
norm1Root = integrateRoot

{- |
prop> withRational $ \x -> G.norm2Root x == G.normPRoot 2 x
-}
norm2Root :: (Field.C a) => T a -> Root.T a
norm2Root f =
   Root.sqrt $
      Root.fromNumber (amp f)
      `Root.div`
      Root.sqrt (Root.fromNumber $ 2 * c f)

normInfRoot :: (Field.C a) => T a -> Root.T a
normInfRoot f =
   Root.sqrt $ Root.fromNumber $ amp f

{-
I would have liked to test for a monotony of norms.
Unfortunately, it does not hold.

Means contain a division by the size of the domain.
Norms do not have this division.
Means are monotonic with respect to the degree.
Norms are not.
We cannot turn the norms into means since the size of the domain
(the complete real axis) is infinitely large.

prop> :{ withRational $ \x p0 q0 ->
   let p = 1 + abs p0
       q = 1 + abs q0
   in  case compare p q of
          EQ -> G.normPRoot p x == G.normPRoot q x
          LT -> G.normPRoot p x <= G.normPRoot q x
          GT -> G.normPRoot p x >= G.normPRoot q x
:}

This should also fail,
but QuickCheck does not seem to try counterexamples.

prop> :{ withRational $ \x p0 ->
   let p = 1 + abs p0
   in  G.normPRoot p x <= G.normInfRoot x
:}
-}
normPRoot :: (Field.C a) => Rational -> T a -> Root.T a
normPRoot p f =
   Root.sqrt (Root.fromNumber (amp f))
   `Root.div`
   Root.rationalPower (recip (2*p)) (Root.fromNumber (fromRational' p * c f))


-- ToDo: implement NormedSpace.Sum et.al.
norm1 :: (Algebraic.C a) => T a -> a
norm1 f =
   sqrt $ amp f / c f

norm2 :: (Algebraic.C a) => T a -> a
norm2 f =
   sqrt $ amp f / (sqrt $ 2 * c f)

normInf :: (Algebraic.C a) => T a -> a
normInf f =
   sqrt (amp f)

normP :: (Trans.C a) => a -> T a -> a
normP p f =
   sqrt (amp f) * (p * c f) ^? (- recip (2*p))


variance :: (Trans.C a) =>
   T a -> a
variance f =
   recip $ c f * 2*pi

{- |
prop> withRational $ \x (QC.Positive a) -> G.varianceRational (G.dilate a x) == a^2 * G.varianceRational x
prop> withRational $ \x y -> G.varianceRational (G.convolve x y) == G.varianceRational x + G.varianceRational y
-}
varianceRational :: (Field.C a) => T a -> a
varianceRational f = recip $ c f

{- |
prop> Laws.identity G.multiply G.constant . asRational
prop> Laws.commutative G.multiply . asRational
prop> Laws.associative G.multiply . asRational
-}
multiply :: (Ring.C a) =>
   T a -> T a -> T a
multiply f g =
   Cons (amp f * amp g) (c f + c g)

powerRing :: (Trans.C a) =>
   Integer -> T a -> T a
powerRing p f =
   Cons (amp f ^ p) (fromInteger p * c f)

{-
powerField does not makes sense,
since the reciprocal of a Gaussian diverges.
-}

powerAlgebraic :: (Trans.C a) =>
   Rational -> T a -> T a
powerAlgebraic p f =
   Cons (amp f ^/ p) (fromRational' p * c f)

powerTranscendental :: (Trans.C a) =>
   a -> T a -> T a
powerTranscendental p f =
   Cons (amp f ^? p) (p * c f)

{- |
> convolve x y t =
>    integrate $ \s -> x s * y(t-s)

Convergence only for @c f + c g > 0@.

prop> Laws.commutative G.convolve . asRational
prop> Laws.associative G.convolve . asRational

Young inequality:

prop> withRational $ \x y -> G.normInfRoot (G.convolve x y) <= G.norm1Root x `Root.mul` G.normInfRoot y
prop> withRational $ \x y (HoelderConjugates p q) -> G.normInfRoot (G.convolve x y) <= G.normPRoot p x `Root.mul` G.normPRoot q y
prop> withRational $ \x y (YoungConjugates p q r) -> G.normPRoot r (G.convolve x y) <= G.normPRoot p x `Root.mul` G.normPRoot q y
-}
convolve :: (Field.C a) =>
   T a -> T a -> T a
convolve f g =
   let s = c f + c g
   in  Cons
          (amp f * amp g / s)
          (c f * c g / s)

{- |
> fourier x f =
>    integrate $ \t -> x t * cis (-2*pi*t*f)

Convergence only for @c f > 0@.

prop> withRational $ \x y -> G.fourier (G.convolve x y) == G.multiply (G.fourier x) (G.fourier y)
prop> withRational $ \x -> nest 4 G.fourier x == x
prop> withRational $ \x (QC.Positive a) -> G.fourier (G.dilate a x) == G.amplify a (G.shrink a (G.fourier x))
prop> withRational $ \x y -> G.scalarProductRoot x y == G.scalarProductRoot (G.fourier x) (G.fourier y)
-}
fourier :: (Field.C a) =>
   T a -> T a
fourier f =
   Cons (amp f / c f) (recip $ c f)
{-
fourier (t -> exp(-(a*t)^2))
-}

{- |
prop> withRational $ \x (QC.Positive a) (QC.Positive b) -> G.dilate a (G.dilate b x) == G.dilate (a*b) x
prop> withRational $ \x (QC.Positive a) -> G.shrink a x == G.dilate (recip a) x
-}
dilate :: (Field.C a) => a -> T a -> T a
dilate k f =
   Cons (amp f) $ c f / k^2

{- |
prop> withRational $ \x (QC.Positive a) -> G.dilate a (G.shrink a x) == x
prop> withRational $ \x (QC.Positive a) -> G.shrink a (G.dilate a x) == x
-}
shrink :: (Ring.C a) => a -> T a -> T a
shrink k f =
   Cons (amp f) $ c f * k^2

{- |
@amplify k@ scales by @abs k@!
-}
amplify :: (Ring.C a) => a -> T a -> T a
amplify k f =
   Cons (k^2 * amp f) $ c f
