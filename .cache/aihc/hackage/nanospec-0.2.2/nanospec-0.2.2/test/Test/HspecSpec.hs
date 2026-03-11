{-# LANGUAGE PackageImports, CPP #-}
module Main (main) where

import "hspec"   Test.Hspec

#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative
#endif

import qualified Test.Hspec as H
import qualified Control.Exception as E
import           System.Exit
import           System.IO.Silently

main :: IO ()
main = hspec spec

failingSpec :: H.Spec
failingSpec = do
  H.describe "foo" $ do
    H.it "foo 1" success
    H.it "foo 2" failure
    H.describe "bar" $ do
      H.it "bar 1" failure
      H.it "bar 2" success
  H.describe "baz" $ do
    H.it "baz 1" failure
  where
    success = H.expect "success" True
    failure = H.expect "failure" False

runSpec :: H.Spec -> IO [String]
runSpec s = lines <$> capture_ (H.hspec s `E.catch` ignore)
  where
    ignore :: ExitCode -> IO ()
    ignore _ = return ()

spec :: Spec
spec = do

  describe "hspec" $ do
    it "exits with exitFailure if not all examples pass" $ do
      H.hspec failingSpec `shouldThrow` (== ExitFailure 1)

    it "prints a report of the test run" $ do
      runSpec failingSpec `shouldReturn` [
          "/foo/foo 1/ OK"
        , "/foo/foo 2/ FAILED"
        , "failure"
        , "/foo/bar/bar 1/ FAILED"
        , "failure"
        , "/foo/bar/bar 2/ OK"
        , "/baz/baz 1/ FAILED"
        , "failure"
        , "5 example(s), 3 failure(s)"
        ]

  describe "evaluateExpectation" $ do
    it "returns Success if expectation holds" $ do
      H.evaluateExpectation (H.expect "foo" True)
        `shouldReturn` H.Success

    it "returns Failure if expectation does not holds" $ do
      H.evaluateExpectation (H.expect "foo" False)
        `shouldReturn` H.Failure "foo"

    it "returns Failure on uncaught exceptions" $ do
      H.evaluateExpectation (E.throwIO $ E.ErrorCall "foobar")
        `shouldReturn` H.Failure "*** Exception: foobar"

    it "re-throws asynchronous exceptions" $ do
      H.evaluateExpectation (E.throwIO E.UserInterrupt)
        `shouldThrow` (== E.UserInterrupt)

  describe "shouldBe" $ do
    it "succeeds if arguments are equal" $ do
      H.evaluateExpectation (23 `H.shouldBe` (23 :: Int))
        `shouldReturn` H.Success

    it "fails if arguments are not equal" $ do
      H.evaluateExpectation (23 `H.shouldBe` (42 :: Int))
        `shouldReturn` H.Failure "expected: 42\n but got: 23"

  describe "shouldReturn" $ do
    it "succeeds if arguments represent equal values" $ do
      H.evaluateExpectation (return 23 `H.shouldReturn` (23 :: Int))
        `shouldReturn` H.Success

    it "fails if arguments do not represent equal values" $ do
      H.evaluateExpectation (return 23 `H.shouldReturn` (42 :: Int))
        `shouldReturn` H.Failure "expected: 42\n but got: 23"
