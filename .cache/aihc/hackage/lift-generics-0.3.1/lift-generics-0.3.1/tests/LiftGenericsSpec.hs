{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module:      LiftGenericsSpec
Copyright:   (C) 2015-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott

@hspec@ tests for `lift-generics`.
-}
module LiftGenericsSpec (main, spec) where

import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax hiding (newName)
import Language.Haskell.TH.Syntax.Compat
import Language.Haskell.TH.Lift.Generics (genericLift)
#if MIN_VERSION_template_haskell(2,16,0)
import Language.Haskell.TH.Lift.Generics (genericLiftTypedCompat)
#endif
import Test.Hspec
import Types
import Control.Exception (Exception, throw, evaluate)

main :: IO ()
main = hspec spec

description :: String
description = "should equal its lifted counterpart"

data Exc = Exc deriving (Show, Eq)
instance Exception Exc

spec :: Spec
spec = parallel $ do
    describe "genericLift" $ do
        describe "Unit" $ do
            it description $ do
                Unit `shouldBe` $(lift Unit)
                ConE 'Unit `shouldBe` runPureQ (liftQuote Unit)
            it "should throw an exception on undefined" $
                evaluate (runPureQ $ liftQuote (throw Exc :: Unit)) `shouldThrow` (== Exc)
        describe "Product" $
            it description $
                p `shouldBe` $(lift p)
        describe "Sum" $
            it description $
                s `shouldBe` $(lift s)
        describe "Unboxed" $
            it description $
                u `shouldBe` $(lift u)
        describe "Const" $
            it description $
                c `shouldBe` $(lift c)
        describe "Ap" $
            it description $
                z `shouldBe` $(lift z)
        -- We use Tree to check that things work for types imported
        -- from external packages without special distinguished names.
        -- It proved rather tricky to find a good choice. This one's
        -- not really ideal (I'd rather have a non-recursive type), but
        -- it'll do for now.
        describe "Tree" $
            it description $
                w `shouldBe` $(genericLift w)
#if MIN_VERSION_template_haskell(2,16,0)
    describe "genericLiftTyped" $ do
        describe "Unit" $
            it description $ do
                Unit `shouldBe` $$(liftTyped Unit)
                ConE 'Unit `shouldBe` runPureQ (unTypeCode (liftTypedQuote Unit))
        describe "Product" $
            it description $
                p `shouldBe` $$(liftTyped p)
        describe "Sum" $
            it description $
                s `shouldBe` $$(liftTyped s)
        describe "Unboxed" $
            it description $
                u `shouldBe` $$(liftTyped u)
        describe "Const" $
            it description $
                c `shouldBe` $$(liftTyped c)
        describe "Ap" $
            it description $
                z `shouldBe` $$(liftTyped z)
        describe "Tree" $
            it description $
                w `shouldBe` $$(genericLiftTypedCompat w)
#endif
