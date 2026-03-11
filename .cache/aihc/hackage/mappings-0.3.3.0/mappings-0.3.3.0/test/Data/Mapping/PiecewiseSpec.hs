module Data.Mapping.PiecewiseSpec where

import Data.Mapping
import Data.Mapping.Piecewise

import Test.Hspec


spec :: Spec
spec = do

  describe "Piecewise" $ do
    let a = changeAt (0 :: Int) (3 :: Int) 1
    let b = changeAt (0 :: Int) (6 :: Int) 1
    let c = merge (+) a b
    it "has right value at 0" $ do
      act c 0 `shouldBe` 0
    it "has right value at 3" $ do
      act c 3 `shouldBe` 1
    it "has right value at 6" $ do
      act c 6 `shouldBe` 2
    it "has right value at 9" $ do
      act c 9 `shouldBe` 2
