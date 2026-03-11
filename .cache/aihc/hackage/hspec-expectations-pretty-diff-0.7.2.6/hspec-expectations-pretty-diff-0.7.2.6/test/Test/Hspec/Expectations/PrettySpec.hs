{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Test.Hspec.Expectations.PrettySpec (spec) where

import           Control.Exception
import           Test.HUnit.Lang
import           Test.Hspec (Spec, describe, it)
import           Data.Aeson
import           Data.List (isPrefixOf)
import           Data.Text (pack)
import           GHC.Stack (srcLocModule)

import           Test.Hspec.Expectations.Pretty

expectationFailed :: String -> HUnitFailure -> Bool
expectationFailed msg (HUnitFailure _ failureReason) =
  msg == formatFailureReason failureReason

expectationFailedPrefix :: String -> HUnitFailure -> Bool
expectationFailedPrefix msg (HUnitFailure _ failureReason) =
  msg `isPrefixOf` formatFailureReason failureReason

data TestStructure = TestStructure String Int [TestStructure]
  deriving (Show, Eq)

spec :: Spec
spec = do
  describe "shouldBe" $ do
    it "succeeds if arguments are equal" $ do
      "foo" `shouldBe` "foo"

    it "fails if arguments are not equal" $ do
      ("foo" `shouldBe` "bar") `shouldThrow` expectationFailed "\ESC[31m---\ESC[0m\ESC[35m\"bar\"\ESC[0m\n\ESC[32m+++\ESC[0m\ESC[35m\"foo\"\ESC[0m\n"
      ([ TestStructure "outer" 123 [ TestStructure "inner" 456 [] ] ] `shouldBe` [ TestStructure "outer" 123 [ TestStructure "inner" 457 [] ] ]) `shouldThrow` expectationFailed "   \ESC[31m[\ESC[0m\ESC[0m\n   \ESC[0m\ESC[0m    \ESC[0m\ESC[0mTestStructure\ESC[0m\ESC[0m \ESC[0m\ESC[35m\"outer\"\ESC[0m\ESC[0m \ESC[0m\ESC[35m123\ESC[0m\ESC[0m \ESC[0m\ESC[31m[\ESC[0m\ESC[0m\n\ESC[31m---\ESC[0m\ESC[0m\ESC[0m        \ESC[0m\ESC[0mTestStructure\ESC[0m\ESC[0m \ESC[0m\ESC[35m\"inner\"\ESC[0m\ESC[0m \ESC[0m\ESC[35m457\ESC[0m\ESC[0m \ESC[0m\ESC[0m[]\ESC[0m\ESC[0m\n\ESC[32m+++\ESC[0m\ESC[0m\ESC[0m        \ESC[0m\ESC[0mTestStructure\ESC[0m\ESC[0m \ESC[0m\ESC[35m\"inner\"\ESC[0m\ESC[0m \ESC[0m\ESC[35m456\ESC[0m\ESC[0m \ESC[0m\ESC[0m[]\ESC[0m\ESC[0m\n   \ESC[0m\ESC[0m    \ESC[0m\ESC[31m]\ESC[0m\ESC[0m\n   \ESC[0m\ESC[31m]\ESC[0m\n"
      (object [ pack "foo" .= object [ pack "bar" .= Number 123 ] ] `shouldBe` object [ pack "foo" .= object [ pack "bar" .= Number 234, pack "quux" .= Number 567 ] ]) `shouldThrow` expectationFailed "   \ESC[0mObject\ESC[0m\ESC[0m \ESC[0m\ESC[36m(\ESC[0m\ESC[0mfromList\ESC[0m\ESC[0m \ESC[0m\ESC[31m[\ESC[0m\ESC[0m\n   \ESC[0m\ESC[0m    \ESC[0m\ESC[36m(\ESC[0m\ESC[35m\"foo\"\ESC[0m\ESC[36m,\ESC[0m\ESC[0m\n   \ESC[0m\ESC[0m    \ESC[0m\ESC[0mObject\ESC[0m\ESC[0m \ESC[0m\ESC[36m(\ESC[0m\ESC[0mfromList\ESC[0m\ESC[0m \ESC[0m\ESC[31m[\ESC[0m\ESC[0m\n\ESC[31m---\ESC[0m\ESC[0m\ESC[0m        \ESC[0m\ESC[36m(\ESC[0m\ESC[35m\"quux\"\ESC[0m\ESC[36m,\ESC[0m\ESC[0m\n\ESC[31m---\ESC[0m\ESC[0m\ESC[0m        \ESC[0m\ESC[0mNumber\ESC[0m\ESC[0m \ESC[0m\ESC[35m567.0\ESC[0m\ESC[36m)\ESC[0m\ESC[36m,\ESC[0m\ESC[0m\n   \ESC[0m\ESC[0m        \ESC[0m\ESC[36m(\ESC[0m\ESC[35m\"bar\"\ESC[0m\ESC[36m,\ESC[0m\ESC[0m\n\ESC[31m---\ESC[0m\ESC[0m\ESC[0m        \ESC[0m\ESC[0mNumber\ESC[0m\ESC[0m \ESC[0m\ESC[35m234.0\ESC[0m\ESC[36m)\ESC[0m\ESC[0m\n\ESC[32m+++\ESC[0m\ESC[0m\ESC[0m        \ESC[0m\ESC[0mNumber\ESC[0m\ESC[0m \ESC[0m\ESC[35m123.0\ESC[0m\ESC[36m)\ESC[0m\ESC[0m\n   \ESC[0m\ESC[0m    \ESC[0m\ESC[31m]\ESC[0m\ESC[36m)\ESC[0m\ESC[36m)\ESC[0m\ESC[0m\n   \ESC[0m\ESC[31m]\ESC[0m\ESC[36m)\ESC[0m\n"

    it "includes the test location in the call stack when it fails" $
      ("foo" `shouldBe` "bar") `shouldThrow` \(HUnitFailure srcLocMaybe _) ->
        fmap srcLocModule srcLocMaybe == Just "Test.Hspec.Expectations.PrettySpec"

  describe "shouldSatisfy" $ do
    it "succeeds if value satisfies predicate" $ do
      "" `shouldSatisfy` null

    it "fails if value does not satisfy predicate" $ do
      ("foo" `shouldSatisfy` null) `shouldThrow` expectationFailed "predicate failed on: \"foo\""

  describe "shouldReturn" $ do
    it "succeeds if arguments represent equal values" $ do
      return "foo" `shouldReturn` "foo"

    it "fails if arguments do not represent equal values" $ do
      (return "foo" `shouldReturn` "bar") `shouldThrow` expectationFailed "\ESC[31m---\ESC[0m\ESC[35m\"bar\"\ESC[0m\n\ESC[32m+++\ESC[0m\ESC[35m\"foo\"\ESC[0m\n"

  describe "shouldStartWith" $ do
    it "succeeds if second is prefix of first" $ do
      "hello world" `shouldStartWith` "hello"

    it "fails if second is not prefix of first" $ do
      ("hello world" `shouldStartWith` "world") `shouldThrow` expectationFailed "\"hello world\" does not start with \"world\""

  describe "shouldEndWith" $ do
    it "succeeds if second is suffix of first" $ do
      "hello world" `shouldEndWith` "world"

    it "fails if second is not suffix of first" $ do
      ("hello world" `shouldEndWith` "hello") `shouldThrow` expectationFailed "\"hello world\" does not end with \"hello\""

  describe "shouldContain" $ do
    it "succeeds if second argument is contained in the first" $ do
      "I'm an hello world message" `shouldContain` "an hello"

    it "fails if first argument does not contain the second" $ do
      ("foo" `shouldContain` "bar") `shouldThrow` expectationFailed "\"foo\" does not contain \"bar\""

  describe "shouldNotBe" $ do
    it "succeeds if arguments are not equal" $ do
      "foo" `shouldNotBe` "bar"

    it "fails if arguments are equal" $ do
      ("foo" `shouldNotBe` "foo") `shouldThrow` expectationFailed "not expected: \"foo\""

  describe "shouldNotSatisfy" $ do
    it "succeeds if value does not satisfy predicate" $ do
      "bar" `shouldNotSatisfy` null

    it "fails if the value does satisfy predicate" $ do
      ("" `shouldNotSatisfy` null) `shouldThrow` expectationFailed "predicate succeded on: \"\""

  describe "shouldNotReturn" $ do
    it "succeeds if arguments does not represent equal values" $ do
      return "foo" `shouldNotReturn` "bar"

    it "fails if arguments do represent equal values" $ do
      (return "foo" `shouldNotReturn` "foo") `shouldThrow` expectationFailed "not expected: \"foo\""

  describe "shouldNotContain" $ do
    it "succeeds if second argument is not contained in the first" $ do
      "I'm an hello world message" `shouldNotContain` "test"

    it "fails if first argument does contain the second" $ do
      ("foo abc def" `shouldNotContain` "def") `shouldThrow` expectationFailed "\"foo abc def\" does contain \"def\""

  describe "shouldThrow" $ do
    it "can be used to require a specific exception" $ do
      throwIO DivideByZero `shouldThrow` (== DivideByZero)

    it "can be used to require any exception" $ do
      error "foobar" `shouldThrow` anyException

    it "can be used to require an exception of a specific type" $ do
      error "foobar" `shouldThrow` anyErrorCall

    it "can be used to require a specific exception" $ do
      error "foobar" `shouldThrow` errorCall "foobar"

    it "fails, if a required specific exception is not thrown" $ do
      (throwIO Overflow `shouldThrow` (== DivideByZero)) `shouldThrow` expectationFailed "predicate failed on expected exception: ArithException (arithmetic overflow)"

    it "fails, if any exception is required, but no exception occurs" $ do
      (return () `shouldThrow` anyException) `shouldThrow` expectationFailed "did not get expected exception: SomeException"

    it "fails, if a required exception of a specific type is not thrown" $ do
      (return () `shouldThrow` anyErrorCall) `shouldThrow` expectationFailed "did not get expected exception: ErrorCall"

    it "fails, if a required specific exception is not thrown" $ do
      (error "foo" `shouldThrow` errorCall "foobar") `shouldThrow` expectationFailedPrefix "predicate failed on expected exception: ErrorCall (foo"
