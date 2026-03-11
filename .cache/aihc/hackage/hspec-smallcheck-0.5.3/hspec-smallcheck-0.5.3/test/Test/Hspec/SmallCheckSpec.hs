{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.SmallCheckSpec (main, spec) where

import           Test.Hspec

import           Data.Orphans ()
import qualified Control.Exception as E

import qualified Test.Hspec.Core.Spec as H
import qualified Test.Hspec.Runner as H
import           Test.SmallCheck
import           Test.QuickCheck (stdArgs)
import           Test.HUnit (Assertion, assertFailure, assertEqual)

import           Test.Hspec.SmallCheck

main :: IO ()
main = hspec spec

exceptionEq :: E.SomeException -> E.SomeException -> Bool
exceptionEq a b
  | Just ea <- E.fromException a, Just eb <- E.fromException b = ea == (eb :: E.ErrorCall)
  | Just ea <- E.fromException a, Just eb <- E.fromException b = ea == (eb :: E.ArithException)
  | otherwise = undefined

deriving instance Eq H.FailureReason
deriving instance Eq H.ResultStatus
deriving instance Eq H.Result

instance Eq E.SomeException where
  (==) = exceptionEq

spec :: Spec
spec = do
  describe "evaluateExample" $ do
    context "with Property IO" $ do
      it "returns Success if property holds" $ do
        eval True `shouldReturn` H.Result "" H.Success

      it "returns Failure if property does not hold" $ do
        eval False `shouldReturn` H.Result "" (H.Failure Nothing (H.Reason "condition is false"))

      it "shows what falsified it" $ do
        eval (/= (2 :: Int)) `shouldReturn` H.Result "" (H.Failure Nothing (H.Reason "there exists 2 such that\n  condition is false"))

      it "propagates exceptions" $ do
        eval (error "foobar" :: Property IO) `shouldThrow` errorCall "foobar"

      context "with HUnit Assertion" $ do
        it "includes failure reason" $ do
          H.Result "" (H.Failure _loc reason) <- eval ((\ _ -> assertFailure "some failure") :: Int -> Assertion)
          reason `shouldBe` H.Reason "there exists 0 such that\nsome failure"

        context "with assertEqual" $ do
          it "includes actual and expected" $ do
            H.Result "" (H.Failure _loc reason) <- eval (assertEqual "foo" (42 :: Int))
            reason `shouldBe` H.ExpectedButGot (Just "there exists 0 such that\nfoo") "42" "0"
  where
    eval :: Testable IO a => a -> IO H.Result
    eval = evaluateExample . property

    evaluateExample :: (Example a, Arg a ~ ()) => a -> IO H.Result
    evaluateExample e = H.evaluateExample e defaultParams ($ ()) (const $ return ())

    defaultParams :: H.Params
    defaultParams = H.Params stdArgs (H.configSmallCheckDepth H.defaultConfig)
