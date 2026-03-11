module Numeric.MantissaSpec (spec) where

import Data.Ratio ((%))
import Test.Hspec
import Numeric.Mantissa


spec :: Spec
spec = do


  describe "Comparisons" $ do

    let twoSevenths = fromRational (2%7) :: Mantissa
    let sixSevenths = fromRational (6%7) :: Mantissa
    
    it "test == (when true)" $ do
      twoSevenths == twoSevenths `shouldBe` True

    it "test == (when false)" $ do
      twoSevenths == sixSevenths `shouldBe` False

    it "test a <= b (when a < b)" $ do
      twoSevenths <= sixSevenths `shouldBe` True

    it "test a <= b (when a = b)" $ do
      twoSevenths <= twoSevenths `shouldBe` True

    it "test a <= b (when a > b)" $ do
      sixSevenths <= twoSevenths `shouldBe` False

    it "test a >= b (when a < b)" $ do
      twoSevenths >= sixSevenths `shouldBe` False

    it "test a >= b (when a = b)" $ do
      twoSevenths >= twoSevenths `shouldBe` True

    it "test a >= b (when a > b)" $ do
      sixSevenths >= twoSevenths `shouldBe` True

    it "test a < b (when a < b)" $ do
      twoSevenths < sixSevenths `shouldBe` True

    it "test a < b (when a = b)" $ do
      twoSevenths < twoSevenths `shouldBe` False

    it "test a < b (when a > b)" $ do
      sixSevenths < twoSevenths `shouldBe` False

    it "test a > b (when a < b)" $ do
      twoSevenths > sixSevenths `shouldBe` False

    it "test a > b (when a = b)" $ do
      twoSevenths > twoSevenths `shouldBe` False

    it "test a > b (when a > b)" $ do
      sixSevenths > twoSevenths `shouldBe` True

  
  describe "Basic arithmetic" $ do

    it "addition" $ do
      mantissaToFractional (fromRational (1%3) + fromRational (1%5)) `shouldBe`
        (fromRational (8%15) :: Float)
      
    it "subtraction" $ do
      mantissaToFractional (fromRational (1%3) - fromRational (1%5)) `shouldBe`
        (fromRational (2%15) :: Float)
      
    it "addition (with wraparound)" $ do
      mantissaToFractional (fromRational (2%3) + fromRational (4%5)) `shouldBe`
        (fromRational (7%15) :: Float)
      
    it "subtraction (with wraparound)" $ do
      mantissaToFractional (fromRational (1%5) - fromRational (1%3)) `shouldBe`
        (fromRational (13%15) :: Float)
      
    it "multiplication" $ do
      mantissaToFractional (fromRational (1%3) * fromRational (1%5)) `shouldBe`
        (fromRational (1%15) :: Float)
      
    it "division" $ do
      mantissaToFractional (fromRational (1%5) / fromRational (1%3)) `shouldBe`
        (fromRational (3%5) :: Float)
