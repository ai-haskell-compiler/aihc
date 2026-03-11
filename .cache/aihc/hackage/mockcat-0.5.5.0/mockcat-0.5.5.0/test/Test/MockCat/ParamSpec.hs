{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.MockCat.ParamSpec (spec) where

import Prelude hiding (and, or)
import Test.Hspec
import Test.MockCat.Cons
import Test.MockCat.Param as P
import Test.MockCat.TH (expectByExpr)

spec :: Spec
spec = do
  describe "Param" do
    describe "|>" do
      it "a |> b" do
        True |> False `shouldBe` param True :> param False
      it "Param a |> b" do
        param True |> False `shouldBe` param True :> param False
      it "a |> Param b" do
        True |> param False `shouldBe` param True :> param False
      it "Param a |> Param b" do
        param True |> param False `shouldBe` param True :> param False
      it "a |> b |> c" do
        True |> False |> True `shouldBe` param True :> (param False :> param True)
      it "Param a |> b |> c" do
        param True |> False |> True `shouldBe` param True :> (param False :> param True)

    describe "Show" do
      it "String" do
        show (param "X") `shouldBe` "X"
      it "Integer" do
        show (param 100) `shouldBe` "100"
      it "Bool" do
        show (param False) `shouldBe` "False"

    describe "Eq" do
      it "param (eq)" do
        param 10 == param 10 `shouldBe` True
      it "param (not eq)" do
        param 10 == param 11 `shouldBe` False

    describe "Returns True if the expected value condition is met." do
      it "any" do
        (P.any :: Param Int) == param 10 `shouldBe` True

    describe "show expectation" do
      it "any" do
        show (P.any :: Param Int) `shouldBe` "any"

      it "expect" do
        show (expect (> 4) "> 4") `shouldBe` "> 4"

      it "expect_" do
        show (expect_ (> 4)) `shouldBe` "[some condition]"
    
    describe "show expectation by Exp" do
      it "(> 3)" do
        show $(expectByExpr [|(> 3)|]) `shouldBe` "(> 3)"

      it "lambda" do
        show $(expectByExpr [|\x -> x == 3 || x == 5|]) `shouldBe` "(\\x -> ((x == 3) || (x == 5)))"
      
      it "use data" do
        show $(expectByExpr [|\x -> x == Foo "foo"|]) `shouldBe` "(\\x -> (x == (Foo \"foo\")))"

data TestData = Foo String | Bar deriving (Eq, Show)