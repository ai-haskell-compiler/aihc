-- Do not edit! Automatically created with doctest-extract from src/MathObj/PartialFraction.hs
{-# LINE 45 "src/MathObj/PartialFraction.hs" #-}

module Test.MathObj.PartialFraction where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 46 "src/MathObj/PartialFraction.hs" #-}
import     qualified MathObj.PartialFraction as PartialFraction
import     qualified MathObj.Polynomial.Core as PolyCore
import     qualified MathObj.Polynomial as Poly
import     qualified Algebra.PrincipalIdealDomain as PID
import     qualified Algebra.Indexable as Indexable
import     qualified Algebra.Laws as Laws
import     qualified Number.Ratio as Ratio
import     Test.NumericPrelude.Utility ((/\))
import     qualified Test.QuickCheck as QC
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base as P
import     Prelude ()

import     Control.Applicative (liftA2)

{-     |
Generator     of irreducible elements for tests.
Choosing     from a list of examples is a simple yet effective design.
If     we would construct irreducible elements by a clever algorithm
we     might obtain multiple primes only rarely.
-}     --
genSmallPrime     :: QC.Gen Integer
genSmallPrime     =
       let primes = [2,3,5,7,11,13]
       in  QC.elements (primes ++ map negate primes)

genPartialFractionInt     :: QC.Gen (PartialFraction.T Integer)
genPartialFractionInt     =
       liftA2 PartialFraction.fromFactoredFraction
          (QC.listOf genSmallPrime) QC.arbitrary


genIrreduciblePolynomial     :: QC.Gen (Poly.T Rational)
genIrreduciblePolynomial     = do
       QC.NonZero unit <- QC.arbitrary
       fmap (Poly.fromCoeffs . map (unit*)) $
          QC.elements [[2,3],[2,0,1],[3,0,1],[1,-3,0,1]]

genPartialFractionPoly     :: QC.Gen (PartialFraction.T (Poly.T Rational))
genPartialFractionPoly     =
       liftA2 PartialFraction.fromFactoredFraction
          (fmap (take 3) $ QC.listOf genIrreduciblePolynomial)
          (fmap (Poly.fromCoeffs . PolyCore.normalize . take 5) QC.arbitrary)


fractionConv     :: (PID.C a, Indexable.C a) => [a] -> a -> Bool
fractionConv     xs y =
       PartialFraction.toFraction (PartialFraction.fromFactoredFraction xs y) ==
       y % product xs

fractionConvAlt     :: (PID.C a, Indexable.C a) => [a] -> a -> Bool
fractionConvAlt     xs y =
       PartialFraction.fromFactoredFraction xs y ==
       PartialFraction.fromFactoredFractionAlt xs y

scaleInt     :: (PID.C a, Indexable.C a) => a -> PartialFraction.T a -> Bool
scaleInt     k a =
       PartialFraction.toFraction (PartialFraction.scaleInt k a) ==
       Ratio.scale k (PartialFraction.toFraction a)

add,     sub, mul ::
       (PID.C a, Indexable.C a) =>
       PartialFraction.T a -> PartialFraction.T a -> Bool
add     = Laws.homomorphism PartialFraction.toFraction (+) (+)
sub     = Laws.homomorphism PartialFraction.toFraction (-) (-)
mul     = Laws.homomorphism PartialFraction.toFraction (*) (*)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.PartialFraction:195: "
{-# LINE 195 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 195 "src/MathObj/PartialFraction.hs" #-}
     (QC.listOf genSmallPrime /\ fractionConv)
 DocTest.printPrefix "MathObj.PartialFraction:196: "
{-# LINE 196 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 196 "src/MathObj/PartialFraction.hs" #-}
     (fmap (take 3) (QC.listOf genIrreduciblePolynomial) /\ fractionConv)
 DocTest.printPrefix "MathObj.PartialFraction:220: "
{-# LINE 220 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 220 "src/MathObj/PartialFraction.hs" #-}
     (QC.listOf genSmallPrime /\ fractionConvAlt)
 DocTest.printPrefix "MathObj.PartialFraction:221: "
{-# LINE 221 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 221 "src/MathObj/PartialFraction.hs" #-}
     (fmap (take 3) (QC.listOf genIrreduciblePolynomial) /\ fractionConvAlt)
 DocTest.printPrefix "MathObj.PartialFraction:297: "
{-# LINE 297 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 297 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionInt /\ \x -> genPartialFractionInt /\ \y -> add x y)
 DocTest.printPrefix "MathObj.PartialFraction:298: "
{-# LINE 298 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 298 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionInt /\ \x -> genPartialFractionInt /\ \y -> sub x y)
 DocTest.printPrefix "MathObj.PartialFraction:300: "
{-# LINE 300 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 300 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionPoly /\ \x -> genPartialFractionPoly /\ \y -> add x y)
 DocTest.printPrefix "MathObj.PartialFraction:301: "
{-# LINE 301 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 301 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionPoly /\ \x -> genPartialFractionPoly /\ \y -> sub x y)
 DocTest.printPrefix "MathObj.PartialFraction:429: "
{-# LINE 429 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 429 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionInt /\ \x k -> scaleInt k x)
 DocTest.printPrefix "MathObj.PartialFraction:430: "
{-# LINE 430 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 430 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionPoly /\ \x k -> scaleInt k x)
 DocTest.printPrefix "MathObj.PartialFraction:449: "
{-# LINE 449 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 449 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionInt /\ \x -> genPartialFractionInt /\ \y -> mul x y)
 DocTest.printPrefix "MathObj.PartialFraction:450: "
{-# LINE 450 "src/MathObj/PartialFraction.hs" #-}
 DocTest.property
{-# LINE 450 "src/MathObj/PartialFraction.hs" #-}
     (genPartialFractionPoly /\ \x -> genPartialFractionPoly /\ \y -> mul x y)
