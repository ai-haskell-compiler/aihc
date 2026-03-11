{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Data.TypeNums.RatsSpec where

import Data.TypeNums
import Test.Hspec
import Test.QuickCheck(property)

import Data.Proxy
import Data.Ratio
import GHC.Exts(Proxy#, proxy#)

spec :: Spec
spec = do
  describe "ratVal" $ do
    it "correctly gets the value of a natural number" $
      ratVal (Proxy @3) `shouldBe` 3
    it "correctly gets the value of zero" $
      ratVal (Proxy @0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      ratVal (Proxy @('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      ratVal (Proxy @('Neg 10)) `shouldBe` (-10)
    it "correctly gets the value of a rational with natural numerator" $
      ratVal (Proxy @(3 ':% 2)) `shouldBe` 3 % 2
    it "correctly gets the value of a negative rational" $
      ratVal (Proxy @('Neg 11 ':% 8)) `shouldBe` (-11) % 8
    it "correctly gets the value of a positive rational" $
      ratVal (Proxy @('Pos 16 ':% 25)) `shouldBe` 16 % 25
  describe "ratVal'" $ do
    it "correctly gets the value of a natural number" $
      ratVal' (proxy# :: Proxy# 3) `shouldBe` 3
    it "correctly gets the value of zero" $
      ratVal' (proxy# :: Proxy# 0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      ratVal' (proxy# :: Proxy# ('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      ratVal' (proxy# :: Proxy# ('Neg 10)) `shouldBe` (-10)
    it "correctly gets the value of a rational with natural numerator" $
      ratVal' (proxy# :: Proxy# (3 ':% 2)) `shouldBe` 3 % 2
    it "correctly gets the value of a negative rational" $
      ratVal' (proxy# :: Proxy# ('Neg 11 ':% 8)) `shouldBe` (-11) % 8
    it "correctly gets the value of a positive rational" $
      ratVal' (proxy# :: Proxy# ('Pos 16 ':% 25)) `shouldBe` 16 % 25
  describe "someRatVal" $
    it "fetches the correct value using ratVal" $ property $
      \x -> case someRatVal x of
        SomeRat y -> ratVal y `shouldBe` x
