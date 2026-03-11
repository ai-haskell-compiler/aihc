module Test.Sound.Synthesizer.Plain.Analysis (tests) where

import qualified Synthesizer.Plain.Analysis as Analysis

import qualified Algebra.Algebraic             as Algebraic
import qualified Algebra.RealField             as RealField
import qualified Algebra.Field                 as Field
import qualified Algebra.RealRing              as RealRing

import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Sum       as NormedSum

import qualified MathObj.LaurentPolynomial as LPoly

import qualified Data.NonEmpty as NonEmpty
import Data.List (genericLength)

import qualified Test.QuickCheck as QC
import Test.QuickCheck (quickCheck, Property, (==>))
import Test.Utility (approxEqual)

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


volumeVectorMaximum :: (NormedMax.C y y, RealRing.C y) => [y] -> Bool
volumeVectorMaximum xs =
   Analysis.volumeVectorMaximum xs == Analysis.volumeMaximum xs

volumeVectorEuclidean ::
   (NormedEuc.C y y, Algebraic.C y, Eq y) =>
   NonEmpty.T [] y -> Bool
volumeVectorEuclidean xs =
   let ys = NonEmpty.flatten xs
   in  Analysis.volumeVectorEuclidean ys == Analysis.volumeEuclidean ys

volumeVectorEuclideanSqr ::
   (NormedEuc.Sqr y y, Field.C y, Eq y) =>
   NonEmpty.T [] y -> Bool
volumeVectorEuclideanSqr xs =
   let ys = NonEmpty.flatten xs
   in  Analysis.volumeVectorEuclideanSqr ys == Analysis.volumeEuclideanSqr ys

volumeVectorSum ::
   (NormedSum.C y y, RealField.C y) =>
   NonEmpty.T [] y -> Bool
volumeVectorSum xs =
   let ys = NonEmpty.flatten xs
   in  Analysis.volumeVectorSum ys == Analysis.volumeSum ys



bounds :: Ord a => NonEmpty.T [] a -> Bool
bounds xs =
   Analysis.bounds xs  ==  (NonEmpty.minimum xs, NonEmpty.maximum xs)


spread :: RealField.C a => (a,a) -> Bool
spread b =
   sum (map snd (Analysis.spread b)) == one


histogramDiscrete :: NonEmpty.T [] Int -> Bool
histogramDiscrete xs =
   Analysis.histogramDiscreteArray xs ==
   Analysis.histogramDiscreteIntMap xs

withEmptyHistogram ::
   (NonEmpty.T [] y -> (Int, [y])) ->
   [y] -> (Int, [y])
withEmptyHistogram f =
   maybe (error "no bounds", []) f . NonEmpty.fetch

histogramDiscreteLength :: [Int] -> Bool
histogramDiscreteLength xs =
   sum (snd (withEmptyHistogram Analysis.histogramDiscreteIntMap xs))
   ==
   length xs

histogramDiscreteConcat :: [Int] -> [Int] -> Bool
histogramDiscreteConcat xs ys =
   let xHist = withEmptyHistogram Analysis.histogramDiscreteIntMap xs
       yHist = withEmptyHistogram Analysis.histogramDiscreteIntMap ys
       xyHist0 =
          LPoly.add
             (uncurry LPoly.Cons xHist)
             (uncurry LPoly.Cons yHist)
       xyHist1 =
          uncurry LPoly.Cons
             (withEmptyHistogram Analysis.histogramDiscreteIntMap (xs++ys))
   in  if null (LPoly.coeffs xyHist0)
         then LPoly.coeffs xyHist0 == LPoly.coeffs xyHist1
         else xyHist0 == xyHist1


histogramLinear :: NonEmpty.T [] Int -> Bool
histogramLinear xs =
   let ys = fmap fromIntegral xs :: NonEmpty.T [] Double
   in  Analysis.histogramLinearArray ys ==
       Analysis.histogramLinearIntMap ys


histogramLinearLength :: NonEmpty.T [] Int -> Bool
histogramLinearLength xs =
   let ys = fmap fromIntegral xs :: NonEmpty.T [] Double
   in  approxEqual 1e-10
          (genericLength $ NonEmpty.tail ys)
          (sum (snd (Analysis.histogramLinearIntMap ys)))
{-
With eps = 1e-15

Falsifiable, after 83 tests:
-20
[32,-41,11,-25,-17,-27,32,-36,7,-36,38]

Falsifiable, after 78 tests:
10
[-35,-28,-28,-24,-4,-29,-14,-29,-20,7,33,-2,-14,-4,7,-40,-5,-12]
-}



centroid :: (Field.C a, Eq a) => [a] -> Property
centroid xs =
   sum xs /= zero ==>
      Analysis.centroid xs == Analysis.centroidAlt xs
-- Test.QuickCheck.quickCheck (\xs -> sum xs /= 0 Test.QuickCheck.==> propCentroid (xs::[Rational]))

histogramDCOffset :: NonEmpty.T (NonEmpty.T []) Int -> Property
histogramDCOffset xs =
   let x1 = NonEmpty.flatten xs
       x  = NonEmpty.flatten x1
       (offset, hist) = Analysis.histogramDiscreteArray x1
   in  sum x /= 0 ==>
          fromIntegral offset + Analysis.centroid (map fromIntegral hist) ==
          (Analysis.directCurrentOffset (map fromIntegral x) :: Rational)


genSmall :: (Functor f, QC.Arbitrary (f Int)) => QC.Gen (f Int)
genSmall = fmap (fmap (flip mod 1000)) QC.arbitrary

forAllSmall ::
   (Functor f, QC.Arbitrary (f Int), Show (f Int), QC.Testable prop) =>
   (f Int -> prop) -> Property
forAllSmall = QC.forAll genSmall


tests :: [(String, IO ())]
tests =
   ("volumeVectorMaximum", quickCheck (volumeVectorMaximum :: [Rational] -> Bool)) :
   -- quickCheck may fail due to rounding errors, but so far the computation is exactly the same
   ("volumeVectorEuclidean", quickCheck (volumeVectorEuclidean :: NonEmpty.T [] Double -> Bool)) :
   ("volumeVectorEuclideanSqr", quickCheck (volumeVectorEuclideanSqr :: NonEmpty.T [] Rational -> Bool)) :
   ("volumeVectorSum", quickCheck (volumeVectorSum :: NonEmpty.T [] Rational -> Bool)) :
   ("bounds", quickCheck (bounds :: NonEmpty.T [] Rational -> Bool)) :
   ("spread", quickCheck (spread :: (Rational,Rational) -> Bool)) :
   ("histogramDiscrete", quickCheck (forAllSmall histogramDiscrete)) :
   ("histogramDiscreteLength", quickCheck (forAllSmall histogramDiscreteLength)) :
   ("histogramDiscreteConcat",
      quickCheck $ forAllSmall $ \x -> forAllSmall $ \y ->
         histogramDiscreteConcat x y) :
   ("histogramLinear", quickCheck (forAllSmall histogramLinear)) :
   ("histogramLinearLength", quickCheck (forAllSmall histogramLinearLength)) :
   ("centroid", quickCheck (centroid :: [Rational] -> Property)) :
   ("histogramDCOffset", quickCheck (forAllSmall histogramDCOffset)) :
   []
