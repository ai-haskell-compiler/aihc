module Test.Sound.Synthesizer.Plain.Control (tests) where

import qualified Synthesizer.Plain.Control as Control

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.Utility (approxEqualListAbs, approxEqualListRel)

import qualified Data.List.HT as ListHT
import Data.List (transpose)

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


linearRing :: Int -> Int -> Bool
linearRing d y0 =
--   Control.linear d y0  ==  Control.linearMultiscale d y0
   all ListHT.allEqual $ take 100 $ transpose $
      Control.linear d y0 :
      Control.linearMultiscale d y0 :
      Control.linearStable d y0 :
      []

{-
*Synthesizer.Plain.Control> propLinearApprox (-2/3) 2
False

Need a different definition of approximate equality.
-}
linearApprox :: Double -> Double -> Bool
linearApprox d y0 =
   all (approxEqualListAbs (1e-10 * max (abs d) (abs y0))) $
   take 100 $ transpose $
      Control.linear d y0 :
      Control.linearMean d y0 :
      Control.linearMultiscale d y0 :
      Control.linearStable d y0 :
      []

linearExact :: Rational -> Rational -> Bool
linearExact d y0 =
   all ListHT.allEqual $ take 100 $ transpose $
      Control.linear d y0 :
      Control.linearMean d y0 :
      Control.linearMultiscale d y0 :
      Control.linearStable d y0 :
      []

exponential :: Double -> Property
exponential y0 =
   QC.forAll (QC.choose (10,1000)) $ \time ->
   all (approxEqualListRel (1e-10)) $ take 100 $ transpose $
      Control.exponential time y0 :
      Control.exponentialMultiscale time y0 :
      Control.exponentialStable time y0 :
      []

exponential2 :: Double -> Property
exponential2 y0 =
   QC.forAll (QC.choose (10,1000)) $ \time ->
   all (approxEqualListRel (1e-10)) $ take 100 $ transpose $
      Control.exponential2 time y0 :
      Control.exponential2Multiscale time y0 :
      Control.exponential2Stable time y0 :
      []

cosine :: Double -> Double -> Property
cosine t0 t1  =  t0/=t1 ==>
   all (approxEqualListAbs (1e-10)) $
   take 100 $ transpose $
      Control.cosine t0 t1 :
      Control.cosineMultiscale t0 t1 :
      Control.cosineStable t0 t1 :
      []


cubic :: (Rational, (Rational, Rational)) ->
   (Rational, (Rational, Rational)) -> Property
cubic node0 node1  =  fst node0 /= fst node1 ==>
   take 100 (Control.cubicHermite node0 node1)  ==
   take 100 (Control.cubicHermiteStable node0 node1)



tests :: [(String, IO ())]
tests =
   ("linearRing", quickCheck linearRing) :
   ("linearApprox", quickCheck linearApprox) :
   ("linearExact", quickCheck linearExact) :
   ("exponential", quickCheck exponential) :
   ("exponential2", quickCheck exponential2) :
   ("cosine", quickCheck cosine) :
   ("cubic", quickCheck cubic) :
   []
