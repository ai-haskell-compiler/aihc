{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Test.MockCat.ConsSpec (spec) where

import Test.Hspec
import Test.MockCat.Cons

spec :: Spec
spec = do
  describe "Cons" do
    describe "Show" do
      it "2 arity" do
        show ((10 :: Int) :> True) `shouldBe` "10,True"
      it "3 arity" do
        show ("1" :> (False :> [3, 4])) `shouldBe` "\"1\",False,[3,4]"

    describe "Eq" do
      it "2 arity" do
        (1 :: Int) :> "2" `shouldBe` (1 :: Int) :> "2"
      it "3 arity" do
        "1" :> (False :> [3, 4]) `shouldBe` "1" :> (False :> [3, 4])

