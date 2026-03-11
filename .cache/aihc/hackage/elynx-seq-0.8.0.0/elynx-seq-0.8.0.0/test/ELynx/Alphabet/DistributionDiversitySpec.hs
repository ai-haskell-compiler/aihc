-- |
-- Module      :  ELynx.Alphabet.DistributionDiversitySpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Feb 25 13:41:12 2019.
module ELynx.Alphabet.DistributionDiversitySpec
  ( spec,
  )
where

import qualified Data.Vector.Unboxed as V
import ELynx.Alphabet.DistributionDiversity
import ELynx.Tools.Equality
import Test.Hspec

testArr1 :: V.Vector Double
testArr1 = V.replicate 20 0.0

testArr2 :: V.Vector Double
testArr2 = V.fromList [0, 0, 0, 1, 0]

-- Compare results from random array tested with Python functions.
testArr3 :: V.Vector Double
testArr3 = V.fromList [0.3, 0.4, 0.7]

spec :: Spec
spec = do
  describe "entropy" $
    it "calculates entropy of vectors" $ do
      entropy testArr1 `shouldBe` 0.0
      entropy testArr2 `shouldBe` 0.0
      entropy testArr3 `shouldSatisfy` nearlyEq 0.9773805948045555
  describe "kEffEntropy" $
    it "calculates the effective number of used states using entropy" $
      do
        kEffEntropy testArr1 `shouldBe` 1.0
        kEffEntropy testArr2 `shouldBe` 1.0
        kEffEntropy testArr3 `shouldSatisfy` nearlyEq 2.6574860842252765
  describe "homoplasy" $
    it "calculates homoplasy of vectors" $ do
      homoplasy testArr1 `shouldBe` 0.0
      homoplasy testArr2 `shouldBe` 1.0
      homoplasy testArr3 `shouldSatisfy` nearlyEq 0.74
  describe "kEffHomoplasy" $
    it "calculates the effective number of used states using homoplasy" $
      do
        kEffHomoplasy testArr1 `shouldSatisfy` isInfinite
        kEffHomoplasy testArr2 `shouldBe` 1.0
        kEffHomoplasy testArr3 `shouldSatisfy` nearlyEq 1.3513513513513513
