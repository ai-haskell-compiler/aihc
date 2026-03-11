{-# LANGUAGE RebindableSyntax #-}
{-
Complex Gaussian bell multiplied with a polynomial.

In order to make this free of @pi@ factors,
we have to choose @recip (sqrt pi)@
as unit for translations and modulations,
for linear factors and in the differentiation.
-}
{-
ToDo:

* In order to avoid the weird @sqrt pi@ factor,
  use a polynomial expression in @pi@.

* sum of multiple bells using Data.Map from exponent polynomial to coefficient polynomial
  use of Algebra object.

* Discrete Fourier Transform and its eigenvectors

* Use projective geometry in order to support Dirac impulse.
  There are many open questions:
  1. What shall be the product of two Dirac impulses -
     whether they are at the same location or not.
  2. How to organize coefficients
     such that the constant function can be modulated
     and the Dirac impulse can be translated.
-}
module MathObj.Gaussian.Polynomial where

import qualified MathObj.Gaussian.Bell as Bell

import qualified MathObj.LaurentPolynomial as LPoly
import qualified MathObj.Polynomial.Core   as PolyCore
import qualified MathObj.Polynomial        as Poly
import qualified Number.Complex     as Complex

import qualified Algebra.ZeroTestable   as ZeroTestable
import qualified Algebra.Differential   as Differential
import qualified Algebra.Transcendental as Trans
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute       as Absolute
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import qualified Data.Record.HT as Rec
import qualified Data.List as List
import Data.Function.HT (nest, )
import Data.Eq.HT (equating, )
import Data.List.HT (mapAdjacent, )
import Data.Tuple.HT (forcePair, )

import Test.QuickCheck (Arbitrary, arbitrary, )
import Control.Monad (liftM2, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (reverse, )


{- $setup
>>> :set -XRebindableSyntax
>>>
>>> import qualified MathObj.Gaussian.Polynomial as G
>>> import qualified MathObj.Gaussian.Bell as Bell
>>> import qualified MathObj.Polynomial as Poly
>>> import qualified Algebra.Laws as Laws
>>> import qualified Number.Complex as Complex
>>> import Number.Complex ((+:))
>>> import NumericPrelude.Base as P
>>> import NumericPrelude.Numeric as NP
>>> import qualified Test.QuickCheck as QC
>>> import Data.Function.HT (Id, nest)
>>> import Data.Tuple.HT (mapSnd)
>>>
>>> asRational :: Id (G.T Rational)
>>> asRational = id
>>>
>>> withRational :: Id (G.T Rational -> a)
>>> withRational = id
>>>
>>> mulLinear2i :: Id (G.T Rational)
>>> mulLinear2i x =
>>>    x{G.polynomial = Poly.fromCoeffs [0, 0+:2] * G.polynomial x}
>>>
>>> rotateQuarter :: Int -> Id (G.T Rational)
>>> rotateQuarter n =
>>>    G.scaleComplex (negate Complex.imaginaryUnit ^ fromIntegral n)
-}


data T a = Cons {bell :: Bell.T a, polynomial :: Poly.T (Complex.T a)}
   deriving (Show)

instance (Absolute.C a, ZeroTestable.C a, Eq a) => Eq (T a) where
   (==) = equal


{-
Helper data type for 'equal',
that allows to call the (not quite trivial) polynomial equality check.
@RootProduct r a@ represents @sqrt r * a@.
The test using 'signum' works for real numbers,
and I do not know, whether it is correct for other mathematical objects.
However I cannot imagine other mathematical objects,
that make sense at all, here.
Maybe elements of a finite field.
-}
data RootProduct a = RootProduct a a

instance (Absolute.C a, ZeroTestable.C a, Eq a) => Eq (RootProduct a) where
   (RootProduct xr xa) == (RootProduct yr ya)  =
      let xp = xr*xa^2
          yp = yr*ya^2
      in  xp==yp &&
          (isZero xp || signum xa == signum ya)

instance (ZeroTestable.C a) => ZeroTestable.C (RootProduct a) where
   isZero (RootProduct r a) = isZero r || isZero a


{-
The derived Eq is not correct.
We have to combine the amplitude of the bell with the polynomial,
respecting signs and the square root of the bell amplitude.
-}
equal :: (Absolute.C a, ZeroTestable.C a, Eq a) => T a -> T a -> Bool
equal x y =
   let bx = bell x
       by = bell y
       scaleSqr b =
          (\p ->
              (fmap (RootProduct (Bell.amp b) . Complex.real) p,
               fmap (RootProduct (Bell.amp b) . Complex.imag) p))
           . polynomial
   in  Rec.equal
          (equating Bell.c0 :
           equating Bell.c1 :
           equating Bell.c2 :
           [])
          bx by
       &&
       scaleSqr bx x == scaleSqr by y


instance (Absolute.C a, ZeroTestable.C a, Arbitrary a) => Arbitrary (T a) where
   arbitrary =
--      liftM2 Cons arbitrary arbitrary
      liftM2 Cons
         arbitrary
         -- we have to restrict the number of polynomial coefficients,
         -- since with the quadratic time algorithms like fourier and convolve,
         -- in connection with Rational slow down tests too much.
         (fmap (Poly.fromCoeffs . take 5 . Poly.coeffs) arbitrary)



{-# INLINE evaluateSqRt #-}
evaluateSqRt :: (Trans.C a) =>
   T a -> a -> Complex.T a
evaluateSqRt f x =
   Bell.evaluateSqRt (bell f) x *
   Poly.evaluate (polynomial f) (Complex.fromReal $ sqrt pi * x)
{- ToDo: evaluating a complex polynomial for a real argument can be optimized -}


constant :: (Ring.C a) => T a
constant =
   Cons Bell.constant (Poly.const one)

scale :: (Ring.C a) => a -> T a -> T a
scale x f =
   f{polynomial = fmap (Complex.scale x) $ polynomial f}

scaleComplex :: (Ring.C a) => Complex.T a -> T a -> T a
scaleComplex x f =
   f{polynomial = fmap (x*) $ polynomial f}


unit :: (Ring.C a) => T a
unit = eigenfunction0

{- |
This one does not hold for larger degrees, although it would be nice:

prop> QC.forAll (QC.choose (0,3)) $ \n -> G.eigenfunctionDifferential n == asRational (G.eigenfunctionIterative n)

Unfortunately, both implementations compute different eigenbases.
-}
eigenfunction :: (Field.C a) => Int -> T a
eigenfunction =
   eigenfunctionDifferential

-- | prop> G.eigenfunction0  ==  asRational (G.eigenfunctionDifferential 0)
eigenfunction0 :: (Ring.C a) => T a
eigenfunction0 =
   Cons Bell.unit (Poly.fromCoeffs [one])

-- | prop> G.eigenfunction1  ==  asRational (G.eigenfunctionDifferential 1)
eigenfunction1 :: (Ring.C a) => T a
eigenfunction1 =
   Cons Bell.unit (Poly.fromCoeffs [zero, one])

-- | prop> G.eigenfunction2  ==  asRational (G.eigenfunctionDifferential 2)
eigenfunction2 :: (Field.C a) => T a
eigenfunction2 =
   Cons Bell.unit (Poly.fromCoeffs [-(1/4), zero, one])

-- | prop> G.eigenfunction3  ==  asRational (G.eigenfunctionDifferential 3)
eigenfunction3 :: (Field.C a) => T a
eigenfunction3 =
   Cons Bell.unit (Poly.fromCoeffs [zero, -(3/4), zero, one])


{- |
prop> QC.forAll (QC.choose (0,15)) $ \n -> let x = G.eigenfunctionDifferential n in G.fourier x  ==  rotateQuarter n x
-}
eigenfunctionDifferential :: (Field.C a) => Int -> T a
eigenfunctionDifferential n =
   (\f -> f{bell = Bell.unit}) $
   nest n (scale (-1/4) . differentiate) $
   Cons (Bell.Cons one zero zero 2) one

{- |
prop> QC.forAll (QC.choose (0,15)) $ \n -> let x = G.eigenfunctionIterative n in G.fourier x  ==  rotateQuarter n x
-}
eigenfunctionIterative ::
   (Field.C a, Absolute.C a, ZeroTestable.C a, Eq a) => Int -> T a
eigenfunctionIterative n =
   fst . head . dropWhile (uncurry (/=)) . mapAdjacent (,) $
   eigenfunctionIteration $
   Cons
      Bell.unit
      (Poly.fromCoeffs $ replicate n zero ++ [one])

eigenfunctionIteration :: (Field.C a) => T a -> [T a]
eigenfunctionIteration =
   iterate (\x ->
      let y = fourier x
          px = polynomial x
          py = polynomial y
          c = last (Poly.coeffs px) / last (Poly.coeffs py)
      in  y{polynomial = fmap (0.5*) (px + fmap (c*) py)})


{- |
prop> withRational $ Laws.identity G.multiply G.constant
prop> withRational $ Laws.commutative G.multiply
prop> withRational $ Laws.associative G.multiply
-}
multiply :: (Ring.C a) =>
   T a -> T a -> T a
multiply f g =
   Cons
      (Bell.multiply (bell f) (bell g))
      (polynomial f * polynomial g)

{- |
prop> withRational $ Laws.commutative G.convolve
prop> withRational $ Laws.associative G.convolve
-}
convolve, {- convolveByDifferentiation, -} convolveByFourier :: (Field.C a) =>
   T a -> T a -> T a
convolve = convolveByFourier

{-
f <*> g =
   let (foff,fint) = integrate f
   in  fint <*> differentiate g + makeGaussPoly foff * g

In principle this would work,
but (makeGaussPoly foff * g) contains a lot of
convolutions of Gaussian with Gaussian-polynomial-product,
where the Gaussians have different parameters.

convolveByDifferentiation f g =
   case polynomial f of
      fpoly ->
         if null $ Poly.coeffs fpoly
           then ...
           else ...
-}

convolveByFourier f g =
   reverse $ fourier $ multiply (fourier f) (fourier g)

{-
We use a Horner like scheme
in order to translate multiplications with @id@
to differentations on the Fourier side.
Quadratic runtime.

fourier (Cons bell (Poly.const a + Poly.shift f))
  = fourier (Cons bell (Poly.const a)) + fourier (Cons bell (Poly.shift f))
  = fourier (Cons bell (Poly.const a)) + differentiate (fourier (Cons bell f))

We can certainly speed this up considerably
by decomposing the polynomial into four polynomials,
one for each of the four eigenvalues 1, i, -1, -i.
-}
{- |
prop> withRational $ \x y -> G.fourier (G.convolve x y) == G.multiply (G.fourier x) (G.fourier y)
prop> withRational $ \x -> nest 2 G.fourier x == G.reverse x
prop> withRational $ \x a -> G.fourier (G.translate a x) == G.modulate a (G.fourier x)
prop> withRational $ \x (QC.Positive a) -> G.fourier (G.dilate a x) == G.amplify a (G.shrink a (G.fourier x))
prop> withRational $ \x -> G.fourier (G.differentiate x) == mulLinear2i (G.fourier x)
-}
fourier :: (Field.C a) =>
   T a -> T a
fourier f =
   foldr
      (\c p ->
          let q = differentiate p
          in  q{polynomial =
                   Poly.const c +
                   fmap (Complex.scale (1/2) . Complex.quarterLeft) (polynomial q)})
      (Cons (Bell.fourier $ bell f) zero) $
   Poly.coeffs $ polynomial f

{- |
Differentiate and divide by @sqrt pi@ in order to stay in a ring.
This way, we do not need to fiddle with pi factors.

prop> withRational $ \x y -> G.convolve (G.differentiate x) y == G.convolve x (G.differentiate y)
-}
differentiate :: (Ring.C a) => T a -> T a
differentiate f =
   f{polynomial =
        Differential.differentiate (polynomial f)
        - Differential.differentiate (Bell.exponentPolynomial (bell f))
           * polynomial f}

{-
g = (bell f * poly f)'
  = bell f * ((poly f)' - (exppoly (bell f))' * poly f)
poly g = (poly f)' - (exppoly (bell f))' * poly f

Integration means we have g and ask for f.

poly f = ((poly f)' - poly g) / (exppoly (bell f))'

However must start with the highest term of 'poly f',
and thus we need to perform the division on reversed polynomials.
-}
{- |
>>> snd $ G.integrate $ G.differentiate $ G.Cons Bell.unit (Poly.fromCoeffs [7,7,7,7 :: Complex.T Rational])
Cons {bell = Cons {amp = 1 % 1, c0 = 0 % 1 +: 0 % 1, c1 = 0 % 1 +: 0 % 1, c2 = 1 % 1}, polynomial = Polynomial.fromCoeffs [7 % 1 +: 0 % 1,7 % 1 +: 0 % 1,7 % 1 +: 0 % 1,7 % 1 +: 0 % 1]}

prop> withRational $ \x -> G.integrate (G.differentiate x) == (zero, x)
prop> withRational $ \x@(G.Cons b p) -> let (xoff,xint) = G.integrate x in G.differentiate xint == G.Cons b (p + Poly.const xoff)
-}
integrate ::
   (Field.C a, ZeroTestable.C a) =>
   T a -> (Complex.T a, T a)
integrate f =
   let fs = Poly.coeffs $ polynomial f
       (ys,~[r]) =
          PolyCore.divModRev
             {-
             We need the shortening convention of 'zipWith'
             in order to limit the result list,
             we cannot use list instance for (-).
             -}
             (zipWith (-)
                (0 : 0 : diffRev ys)
                (List.reverse fs))
             (List.reverse $ Poly.coeffs $
              Differential.differentiate $
              Bell.exponentPolynomial $ bell f)
   in  forcePair $
       if null fs
         then (zero, f)
         else (r, f{polynomial = Poly.fromCoeffs $ List.reverse ys})

diffRev :: Ring.C a => [a] -> [a]
diffRev xs =
   zipWith (*) xs
      (drop 1 (iterate (subtract 1) (fromIntegral $ length xs)))

{-
integrateDefinite
   (maybe rename integrate to antiderivative and call this one integrate)

int(x^(2*n)*exp(-x^2),x=-infinity..infinity)
 = 2 * int(x^(2*n)*exp(-x^2),x=0..infinity)
     substitute t=x^2, dt = dx * 2 * sqrt t
 = int(t^(n-1/2)*exp(-t),x=0..infinity)
 = Gamma(n+1/2)
 = (2n-1)!!/2^n * sqrt pi

int(pi^n*x^(2*n)*exp(-pi*x^2),x=-infinity..infinity)
 = (2n-1)!!/2^n


The remainder value of 'integrate'
is the coefficient of the error function
and this is the only part that does not vanish when approaching the limit.


In order to stay in a field,
we have to return a rational number
and a transcendental part written es @exp a@.

It would be interesting to see how integral inequalities
translate to scalar inequalities containing exponential functions.
-}


{- |
prop> withRational $ \x a b -> G.translate a (G.translate b x) == G.translate (a+b) x
-}
translate :: Ring.C a => a -> T a -> T a
translate d =
   translateComplex (Complex.fromReal d)

{- |
prop> withRational $ \x a b -> G.translateComplex a (G.translateComplex b x) == G.translateComplex (a+b) x
prop> withRational $ \x a -> G.translateComplex (Complex.fromReal a) x == G.translate a x
-}
translateComplex :: Ring.C a => Complex.T a -> T a -> T a
translateComplex d f =
   Cons
      (Bell.translateComplex d $ bell f)
      (Poly.translate d $ polynomial f)

{- |
prop> withRational $ \x a b -> G.modulate a (G.modulate b x) == G.modulate (a+b) x
prop> withRational $ \x a b -> G.modulate b (G.translate a x) == G.turn (a*b) (G.translate a (G.modulate b x))
-}
modulate :: Ring.C a => a -> T a -> T a
modulate d f =
   Cons
      (Bell.modulate d $ bell f)
      (polynomial f)

turn :: Ring.C a => a -> T a -> T a
turn d f =
   Cons
      (Bell.turn d $ bell f)
      (polynomial f)

{- |
prop> withRational $ \x -> nest 2 G.reverse x == x
-}
reverse :: Additive.C a => T a -> T a
reverse f =
   Cons
      (Bell.reverse $ bell f)
      (Poly.reverse $ polynomial f)

{- |
prop> withRational $ \x (QC.Positive a) (QC.Positive b) -> G.dilate a (G.dilate b x) == G.dilate (a*b) x
prop> withRational $ \x (QC.Positive a) -> G.shrink a x == G.dilate (recip a) x
-}
dilate :: Field.C a => a -> T a -> T a
dilate k f =
   Cons
      (Bell.dilate k $ bell f)
      (Poly.dilate (Complex.fromReal k) $ polynomial f)

{- |
prop> withRational $ \x (QC.Positive a) -> G.dilate a (G.shrink a x) == x
prop> withRational $ \x (QC.Positive a) -> G.shrink a (G.dilate a x) == x
-}
shrink :: Ring.C a => a -> T a -> T a
shrink k f =
   Cons
      (Bell.shrink k $ bell f)
      (Poly.shrink (Complex.fromReal k) $ polynomial f)

{-
We could also amplify the polynomial coefficients.
-}
amplify :: Ring.C a => a -> T a -> T a
amplify k f =
   Cons
      (Bell.amplify k $ bell f)
      (polynomial f)


{- |
Approximate a @T a@ using a linear combination of translated @Bell.T a@.
The smaller the unit (e.g. 0.1, 0.01, 0.001)
the better the approximation but the worse the numeric properties.

We cannot put all information into @amp@ of @Bell@,
since @amp@ must be real, but is complex here by construction.
We really need at least signed amplitudes at this place,
since we want to represent differences of Gaussians.

prop> withRational $ \x (QC.NonZero unit) d -> G.approximateByBells unit (G.translateComplex d x) == map (mapSnd (Bell.translateComplex d)) (G.approximateByBells unit x)
prop> withRational $ \x (QC.NonZero unit) (QC.NonZero d) -> G.approximateByBells unit (G.dilate d x) == map (mapSnd (Bell.dilate d)) (G.approximateByBells (unit/d) x)
prop> withRational $ \x (QC.NonZero unit) (QC.NonZero d) -> G.approximateByBells unit (G.shrink d x) == map (mapSnd (Bell.shrink d)) (G.approximateByBells (unit*d) x)
-}
approximateByBells ::
   Field.C a =>
   a -> T a -> [(Complex.T a, Bell.T a)]
approximateByBells unit_ f =
   let b = bell f
       amps =
          -- approximateByBellsByTranslation
          approximateByBellsAtOnce
             unit_
             (Complex.scale (recip (2 * Bell.c2 b)) (Bell.c1 b))
             (recip (2*unit_*Bell.c2 b))
             (polynomial f)
   in  zip (LPoly.coeffs amps) $
       map
          (\d -> Bell.translate d b)
          (laurentAbscissas (unit_/2) amps)

{- |
prop> \(QC.NonZero unit) d s p0 -> let p = Poly.fromCoeffs $ take 10 p0 in G.approximateByBellsAtOnce unit d s p == G.approximateByBellsByTranslation unit d (s::Rational) p
-}
approximateByBellsAtOnce ::
   Field.C a =>
   a -> Complex.T a -> a -> Poly.T (Complex.T a) -> LPoly.T (Complex.T a)
approximateByBellsAtOnce unit_ d s p =
   foldr
      (\x amps0 ->
         {-
         Decompose (bell t * (t-d)) = bell t * t - bell t * d
         -}
         let y = fmap (Complex.scale s) amps0
         in  -- \t -> bell t * t
             --    ~   (translate unit_ bell - translate (-unit_) bell) / unit_
             LPoly.shift 1 y -
             LPoly.shift (-1) y +
             -- bell t * d
             zipWithAbscissas
                (\t z -> (Complex.fromReal t - d) * z)
                (unit_/2) amps0 +
             LPoly.const x)
      (LPoly.fromCoeffs [])
      (Poly.coeffs p)

approximateByBellsByTranslation ::
   Field.C a =>
   a -> Complex.T a -> a -> Poly.T (Complex.T a) -> LPoly.T (Complex.T a)
approximateByBellsByTranslation unit_ d s p =
   foldr
      (\x amps0 ->
         {-
         Decompose (bell t * (t-d)) = bell t * t - bell t * d
         -}
         let y = fmap (Complex.scale s) amps0
         in  -- \t -> bell t * t
             --    ~   (translate unit_ bell - translate (-unit_) bell) / unit_
             LPoly.shift 1 y -
             LPoly.shift (-1) y +
             -- bell t * d
             zipWithAbscissas Complex.scale (unit_/2) amps0 +
             LPoly.const x)
      (LPoly.fromCoeffs [])
      (Poly.coeffs $ Poly.translate d p)

zipWithAbscissas ::
   (Ring.C a) =>
   (a -> b -> c) -> a -> LPoly.T b -> LPoly.T c
zipWithAbscissas h unit_ y =
   LPoly.fromShiftCoeffs (LPoly.expon y) $
   zipWith h
      (laurentAbscissas unit_ y)
      (LPoly.coeffs y)

laurentAbscissas :: Ring.C a => a -> LPoly.T c -> [a]
laurentAbscissas unit_ =
   map (\d -> fromIntegral d * unit_) .
   iterate (1+) . LPoly.expon


{- No Ring instance for Gaussians
instance (Ring.C a) => Differential.C (T a) where
   differentiate = differentiate
-}

{- laws
differentiate (f*g) =
   (differentiate f) * g + f * (differentiate g)

inequalities:

Heisenberg's uncertainty relation
   needs integrals and thus needs product of exponential numbers and roots
-}
