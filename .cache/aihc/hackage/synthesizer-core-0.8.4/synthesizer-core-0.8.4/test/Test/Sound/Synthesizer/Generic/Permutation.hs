{-
wish list:
 - custom Permutation type with Arbitrary instance
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Test.Sound.Synthesizer.Generic.Permutation (tests) where

import qualified Synthesizer.Generic.Permutation as Permutation

import qualified Test.QuickCheck as QC
import Test.QuickCheck (quickCheck, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


genRelPrime :: QC.Gen (Int, Int)
genRelPrime = do
   (n0,m0) <- QC.arbitrary
   let g = gcd n0 m0
   return $ if g==0 then (0,0) else (abs (div n0 g), abs (div m0 g))

tests :: [(String, IO ())]
tests =
   ("inverse transposition",
      quickCheck $
      QC.forAll (QC.choose (0,100)) $ \n ->
      QC.forAll (QC.choose (0,100)) $ \m ->
         Permutation.inverse (Permutation.transposition n m)
         ==
         Permutation.transposition m n) :
   ("inverse skewGrid",
      quickCheck $
      QC.forAll genRelPrime $ \(n,m) ->
         Permutation.inverse (Permutation.skewGrid n m)
         ==
         Permutation.skewGridInv n m) :
   ("inverse skewGridCRT",
      quickCheck $
      QC.forAll genRelPrime $ \(n,m) ->
         Permutation.inverse (Permutation.skewGridCRT n m)
         ==
         Permutation.skewGridCRTInv n m) :
   {-
   reverse (multiplicative (generator n) n)
   ==
   multiplicative (recip $ generator n) n
   -}
   []
