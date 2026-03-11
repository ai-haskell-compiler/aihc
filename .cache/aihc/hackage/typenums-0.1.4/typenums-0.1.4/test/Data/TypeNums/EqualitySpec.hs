{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Data.TypeNums.EqualitySpec where

import Test.Hspec

import Data.TypeNums
import TestUtil

spec :: Spec
spec = do
  describe "(==?)" $ do
    it "Returns true for Int ==? Int" $
      demote @(Int ==? Int) `shouldBe` True
    it "Returns false for String ==? Int" $
      demote @(String ==? Int) `shouldBe` False
    it "Returns true for String ==? [Char]" $
      demote @(String ==? [Char]) `shouldBe` True
    it "Returns true for 3 ==? Pos 3" $
      demote @(3 ==? Pos 3) `shouldBe` True
    it "Returns false for 3 ==? Neg 3" $
      demote @(3 ==? Neg 3) `shouldBe` False
    it "Returns true for 3 ==? 6%2" $
      demote @(3 ==? 6:%2) `shouldBe` True
    it "Returns true for +3 ==? 3%1" $
      demote @(Pos 3 ==? 3:%1) `shouldBe` True
    it "Returns false for 3 ==? 3%2" $
      demote @(3 ==? 3:%2) `shouldBe` False