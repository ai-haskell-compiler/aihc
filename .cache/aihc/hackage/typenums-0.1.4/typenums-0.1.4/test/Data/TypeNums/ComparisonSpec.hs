{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.TypeNums.ComparisonSpec where

import Data.TypeNums

import Test.Hspec
import TestUtil

spec :: Spec
spec = do
  describe "<=?" $ do
    it "is 'True for Nats 2 <= 3" $
      demote @(2 <=? 3) `shouldBe` True
    it "is 'True for Nats 2 <= 2" $
      demote @(2 <=? 2) `shouldBe` True
    it "is 'False for Nats 3 <= 2" $
      demote @(3 <=? 2) `shouldBe` False

    it "is 'True for Neg <= Pos" $
      demote @(Neg 3 <=? Pos 2) `shouldBe` True
    it "is 'True for Pos 0 <= Neg 0" $
      demote @(Pos 0 <=? Neg 0) `shouldBe` True
    it "is 'False for Pos <= Neg" $
      demote @(Pos 2 <=? Neg 3) `shouldBe` False

    it "is 'True for Nat 2 <= Pos 3" $
      demote @(2 <=? Pos 3) `shouldBe` True
    it "is 'True for Nat 2 <= Pos 2" $
      demote @(2 <=? Pos 2) `shouldBe` True
    it "is 'False for Nat 3 <= Pos 2" $
      demote @(3 <=? Pos 2) `shouldBe` False

    it "is 'True for Pos 2 <= Nat 3" $
      demote @(Pos 2 <=? 3) `shouldBe` True
    it "is 'True for Pos 2 <= Nat 2" $
      demote @(Pos 2 <=? 2) `shouldBe` True
    it "is 'False for Pos 3 <= Nat 2" $
      demote @(Pos 3 <=? 2) `shouldBe` False

    it "is 'True for Pos 2 <= Pos 3" $
      demote @(Pos 2 <=? Pos 3) `shouldBe` True
    it "is 'True for Pos 2 <= Pos 2" $
      demote @(Pos 2 <=? Pos 2) `shouldBe` True
    it "is 'False for Pos 3 <= Pos 2" $
      demote @(Pos 3 <=? Pos 2) `shouldBe` False

    it "is 'True for Nat 0 <= Neg 0" $
      demote @(0 <=? Neg 0) `shouldBe` True
    it "is 'False for Nat <= Neg" $
      demote @(3 <=? Neg 2) `shouldBe` False
    it "is 'True for Neg <= Nat" $
      demote @(Neg 3 <=? 2) `shouldBe` True

    it "is 'True for 2/3 <= 3/2" $
      demote @((2 ':% 3) <=? (3 ':% 2)) `shouldBe` True
    it "is 'False for 3/2 <= 2/3" $
      demote @((3 ':% 2) <=? (2 ':% 3)) `shouldBe` False

    it "is 'True for Neg 3/2 <= 2/3" $
      demote @((Neg 3 ':% 2) <=? (2 ':% 3)) `shouldBe` True
