{-# LANGUAGE RebindableSyntax #-}
module MathObj.RefinementMask2 (
   T, coeffs, fromCoeffs,
   fromPolynomial,
   toPolynomial,
   toPolynomialFast,
   refinePolynomial,
   convolvePolynomial,
   convolveTruncatedPowerPolynomials,
   ) where

import qualified MathObj.Polynomial as Poly
import qualified MathObj.Polynomial.Core as PolyCore
import qualified Algebra.RealField as RealField
import qualified Algebra.Field  as Field
import qualified Algebra.Ring   as Ring
import qualified Algebra.Vector as Vector

import qualified Data.List as List
import qualified Data.List.HT as ListHT
import qualified Data.List.Match as Match
import Data.Maybe (fromMaybe, )
import Control.Monad (liftM2, )

import qualified Test.QuickCheck as QC

import qualified NumericPrelude.List.Generic as NPList
import NumericPrelude.Base
import NumericPrelude.Numeric


{- $setup
>>> import qualified MathObj.RefinementMask2 as Mask
>>> import qualified MathObj.Polynomial      as Poly
>>> import qualified MathObj.Polynomial.Core as PolyCore
>>>
>>> import qualified Algebra.Differential as D
>>> import qualified Algebra.Ring as Ring
>>> import Test.NumericPrelude.Utility ((/\))
>>> import qualified Test.QuickCheck as QC
>>> import NumericPrelude.Numeric as NP
>>> import NumericPrelude.Base as P
>>> import Prelude ()
>>>
>>> import Data.Function.HT (nest)
>>> import Data.Maybe (fromMaybe)
>>>
>>>
>>> hasMultipleZero :: (Ring.C a, Eq a) => Int -> a -> Poly.T a -> Bool
>>> hasMultipleZero n x poly =
>>>    all (zero==) $ take n $
>>>    map (flip Poly.evaluate x) $
>>>    iterate D.differentiate poly
>>>
>>> genAdmissibleMask :: QC.Gen (Mask.T Rational, Poly.T Rational)
>>> genAdmissibleMask =
>>>    QC.suchThatMap QC.arbitrary $
>>>       \mask -> fmap ((,) mask) $ Mask.toPolynomial mask
>>>
>>> polyFromMask :: Mask.T a -> Poly.T a
>>> polyFromMask = Poly.fromCoeffs . Mask.coeffs
>>>
>>> genShortPolynomial :: Int -> QC.Gen (Poly.T Rational)
>>> genShortPolynomial n =
>>>    fmap (Poly.fromCoeffs . PolyCore.normalize . take n) $ QC.arbitrary
-}


newtype T a = Cons {coeffs :: [a]}


{-# INLINE fromCoeffs #-}
fromCoeffs :: [a] -> T a
fromCoeffs = lift0

{-# INLINE lift0 #-}
lift0 :: [a] -> T a
lift0 = Cons

{-
{-# INLINE lift1 #-}
lift1 :: ([a] -> [a]) -> (T a -> T a)
lift1 f (Cons x0) = Cons (f x0)

{-# INLINE lift2 #-}
lift2 :: ([a] -> [a] -> [a]) -> (T a -> T a -> T a)
lift2 f (Cons x0) (Cons x1) = Cons (f x0 x1)
-}

{-
Functor instance is e.g. useful for converting number types,
say 'Rational' to 'Double'.
-}

instance Functor T where
   fmap f (Cons xs) = Cons (map f xs)

{-# INLINE appPrec #-}
appPrec :: Int
appPrec  = 10

instance (Show a) => Show (T a) where
   showsPrec p (Cons xs) =
      showParen (p >= appPrec)
         (showString "RefinementMask2.fromCoeffs " . shows xs)

instance (QC.Arbitrary a, Field.C a) => QC.Arbitrary (T a) where
   arbitrary =
      liftM2
         (\degree body ->
            let s = sum body
            in  Cons $ map ((2 ^- degree - s) / NPList.lengthLeft body +) body)
         (QC.choose (-5,0)) QC.arbitrary


{- |
Determine mask by Gauss elimination.

R - alternating binomial coefficients
L - differences of translated polynomials in columns

p2 = L * R^(-1) * m

R * L^(-1) * p2 = m


prop> genAdmissibleMask /\ \(mask,poly) -> hasMultipleZero (fromMaybe 0 $ Poly.degree poly) 1 (polyFromMask (Mask.fromPolynomial poly) - polyFromMask mask)

prop> genShortPolynomial 5 /\ \poly -> maybe False (Poly.collinear poly) $ Mask.toPolynomial $ Mask.fromPolynomial poly
-}
fromPolynomial ::
   (Field.C a) => Poly.T a -> T a
fromPolynomial poly =
   fromCoeffs $
   foldr (\p ps ->
      ListHT.mapAdjacent (-) (p:ps++[0]))
      [] $
   foldr (\(db,dp) cs ->
      ListHT.switchR
         (error "RefinementMask2.fromPolynomial: polynomial should be non-empty")
         (\dps dpe ->
            cs ++ [(db - Ring.scalarProduct dps cs) / dpe])
         dp) [] $
   zip
      (Poly.coeffs $ Poly.dilate 2 poly)
      (List.transpose $
       Match.take (Poly.coeffs poly) $
       map Poly.coeffs $
       iterate polynomialDifference poly)

polynomialDifference ::
   (Ring.C a) => Poly.T a -> Poly.T a
polynomialDifference poly =
   Poly.fromCoeffs $ init $ Poly.coeffs $
   Poly.translate 1 poly - poly

{- |
If the mask does not sum up to a power of @1/2@
then the function returns 'Nothing'.

>>> fmap ((6::Rational) *>) $ Mask.toPolynomial (Mask.fromCoeffs [0.1, 0.02, 0.005::Rational])
Just (Polynomial.fromCoeffs [-12732 % 109375,272 % 625,-18 % 25,1 % 1])
-}
toPolynomial ::
   (RealField.C a) => T a -> Maybe (Poly.T a)
toPolynomial (Cons []) = Just $ Poly.fromCoeffs []
toPolynomial mask =
   let s = sum $ coeffs mask
       ks = reverse $ takeWhile (<=1) $ iterate (2*) s
   in  case ks of
          1:ks0 ->
             Just $
             foldl
                (\p k ->
                   let ip = Poly.integrate zero p
                   in  ip + Poly.const (correctConstant (fmap (k/s*) mask) ip))
                (Poly.const 1) ks0
          _ -> Nothing

{-
The constant term must be zero,
higher terms must already satisfy the refinement constraint.
-}
correctConstant ::
   (Field.C a) => T a -> Poly.T a -> a
correctConstant mask poly =
   let refined = refinePolynomial mask poly
   in  head (Poly.coeffs refined) / (1 - sum (coeffs mask))

toPolynomialFast ::
   (RealField.C a) => T a -> Maybe (Poly.T a)
toPolynomialFast mask =
   let s = sum $ coeffs mask
       ks = reverse $ takeWhile (<=1) $ iterate (2*) s
   in  case ks of
          1:ks0 ->
             Just $
             foldl
                (\p k ->
                   let ip = Poly.integrate zero p
                       c = head (Poly.coeffs (refinePolynomial mask ip))
                   in  ip + Poly.const (c*k / ((1-k)*s)))
                (Poly.const 1) ks0
          _ -> Nothing

{- |
prop> genShortPolynomial 5 /\ \poly -> poly == Mask.refinePolynomial (Mask.fromPolynomial poly) poly

>>> fmap (round :: Double -> Integer) $ fmap (1000000*) $ nest 50 (Mask.refinePolynomial (Mask.fromCoeffs [0.1, 0.02, 0.005])) (Poly.fromCoeffs [0,0,0,1])
Polynomial.fromCoeffs [-116407,435200,-720000,1000000]
-}
refinePolynomial ::
   (Ring.C a) => T a -> Poly.T a -> Poly.T a
refinePolynomial mask =
   Poly.shrink 2 .
   Vector.linearComb (coeffs mask) .
   iterate (Poly.translate 1)

convolve ::
   (Ring.C a) => T a -> T a -> T a
convolve x y =
   fromCoeffs $
   PolyCore.mul (coeffs x) (coeffs y)

{- |
Convolve polynomials via refinement mask.

(mask x + ux*(-1,1)^degree x) * (mask y + uy*(-1,1)^degree y)
-}
convolvePolynomial ::
   (RealField.C a) =>
   Poly.T a -> Poly.T a -> Poly.T a
convolvePolynomial x y =
   fromMaybe
      (error "RefinementMask2.convolvePolynomial: leading term should always be correct") $
   toPolynomial $ fmap (/2) $
   convolve (fromPolynomial x) (fromPolynomial y)

{-
This function interprets all monomials as truncated power functions,
that is power functions that are set to zero for negative arguments.
However the convolution implied by this interpretation
cannot be represented by means of mask convolution.
See for instance:

*MathObj.RefinementMask2> let x = Poly.fromCoeffs [1,1] :: Poly.T Rational
*MathObj.RefinementMask2> fromPolynomial $ convolvePolynomial2 x x
RefinementMask2.fromCoeffs [1 % 3,-1 % 8,-1 % 8,1 % 24]

The obtained mask cannot be factored,
thus it is not a complete square.
But maybe it becomes a square if we add u*(-1,1)^4.
However this mask has sum 1/8 and the added term has sum 0,
thus the sum of the modified mask is still 1/8 and thus not a square.
-}
convolveTruncatedPowerPolynomials ::
   (RealField.C a) =>
   Poly.T a -> Poly.T a -> Poly.T a
convolveTruncatedPowerPolynomials x y =
   let facs = scanl (*) 1 $ iterate (1+) 1
       xl = Poly.coeffs x
       yl = Poly.coeffs y
   in  Poly.integrate 0 $
       Poly.fromCoeffs $
       zipWith (flip (/)) facs $
       PolyCore.mul
          (zipWith (*) facs xl)
          (zipWith (*) facs yl)
