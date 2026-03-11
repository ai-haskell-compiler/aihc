-- Do not edit! Automatically created with doctest-extract from src/MathObj/RefinementMask2.hs
{-# LINE 32 "src/MathObj/RefinementMask2.hs" #-}

module Test.MathObj.RefinementMask2 where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 33 "src/MathObj/RefinementMask2.hs" #-}
import     qualified MathObj.RefinementMask2 as Mask
import     qualified MathObj.Polynomial      as Poly
import     qualified MathObj.Polynomial.Core as PolyCore

import     qualified Algebra.Differential as D
import     qualified Algebra.Ring as Ring
import     Test.NumericPrelude.Utility ((/\))
import     qualified Test.QuickCheck as QC
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base as P
import     Prelude ()

import     Data.Function.HT (nest)
import     Data.Maybe (fromMaybe)


hasMultipleZero     :: (Ring.C a, Eq a) => Int -> a -> Poly.T a -> Bool
hasMultipleZero     n x poly =
       all (zero==) $ take n $
       map (flip Poly.evaluate x) $
       iterate D.differentiate poly

genAdmissibleMask     :: QC.Gen (Mask.T Rational, Poly.T Rational)
genAdmissibleMask     =
       QC.suchThatMap QC.arbitrary $
          \mask -> fmap ((,) mask) $ Mask.toPolynomial mask

polyFromMask     :: Mask.T a -> Poly.T a
polyFromMask     = Poly.fromCoeffs . Mask.coeffs

genShortPolynomial     :: Int -> QC.Gen (Poly.T Rational)
genShortPolynomial     n =
       fmap (Poly.fromCoeffs . PolyCore.normalize . take n) $ QC.arbitrary

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.RefinementMask2:127: "
{-# LINE 127 "src/MathObj/RefinementMask2.hs" #-}
 DocTest.property
{-# LINE 127 "src/MathObj/RefinementMask2.hs" #-}
     (genAdmissibleMask /\ \(mask,poly) -> hasMultipleZero (fromMaybe 0 $ Poly.degree poly) 1 (polyFromMask (Mask.fromPolynomial poly) - polyFromMask mask))
 DocTest.printPrefix "MathObj.RefinementMask2:129: "
{-# LINE 129 "src/MathObj/RefinementMask2.hs" #-}
 DocTest.property
{-# LINE 129 "src/MathObj/RefinementMask2.hs" #-}
     (genShortPolynomial 5 /\ \poly -> maybe False (Poly.collinear poly) $ Mask.toPolynomial $ Mask.fromPolynomial poly)
 DocTest.printPrefix "MathObj.RefinementMask2:161: "
{-# LINE 161 "src/MathObj/RefinementMask2.hs" #-}
 DocTest.example
{-# LINE 161 "src/MathObj/RefinementMask2.hs" #-}
   (fmap ((6::Rational) *>) $ Mask.toPolynomial (Mask.fromCoeffs [0.1, 0.02, 0.005::Rational]))
  [ExpectedLine [LineChunk "Just (Polynomial.fromCoeffs [-12732 % 109375,272 % 625,-18 % 25,1 % 1])"]]
 DocTest.printPrefix "MathObj.RefinementMask2:207: "
{-# LINE 207 "src/MathObj/RefinementMask2.hs" #-}
 DocTest.property
{-# LINE 207 "src/MathObj/RefinementMask2.hs" #-}
     (genShortPolynomial 5 /\ \poly -> poly == Mask.refinePolynomial (Mask.fromPolynomial poly) poly)
 DocTest.printPrefix "MathObj.RefinementMask2:209: "
{-# LINE 209 "src/MathObj/RefinementMask2.hs" #-}
 DocTest.example
{-# LINE 209 "src/MathObj/RefinementMask2.hs" #-}
   (fmap (round :: Double -> Integer) $ fmap (1000000*) $ nest 50 (Mask.refinePolynomial (Mask.fromCoeffs [0.1, 0.02, 0.005])) (Poly.fromCoeffs [0,0,0,1]))
  [ExpectedLine [LineChunk "Polynomial.fromCoeffs [-116407,435200,-720000,1000000]"]]
