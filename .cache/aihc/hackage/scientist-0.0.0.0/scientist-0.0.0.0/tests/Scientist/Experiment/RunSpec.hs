{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scientist.Experiment.RunSpec
  ( spec
  ) where

import Scientist.Test

import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import Scientist.Candidate
import Scientist.Control
import Scientist.Duration
import Scientist.Experiment
import Scientist.Experiment.Run
import Scientist.Result
import UnliftIO.Concurrent
import UnliftIO.Exception (fromException, throwString)

data ExampleResult = A | B | C
  deriving stock (Eq, Show)

spec :: Spec
spec = do
  describe "experimentRun" $ do
    it "is ResultSkipped based on runIf" $ do
      result <-
        experimentRunInternal
        $ setExperimentRunIf False
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      expectSkippedWith result A

    it "is ResultSkipped based on enabled" $ do
      result <-
        experimentRunInternal
        $ setExperimentEnabled (pure False)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      expectSkippedWith result A

    it "is ResultSkipped when no Candidates present" $ do
      result <- experimentRunInternal $ newExperiment "test" (pure A)

      expectSkippedWith result A

    it "is ResultMatched if all candidates match" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTry (pure A)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (A <$ threadDelay (100 * 1000))

      expectMatched result $ \rd -> do
        resultDetailsExperimentName rd `shouldBe` "test"

        let control = resultDetailsControl rd
        resultControlValue control `shouldBe` A
        resultControlDuration control `shouldSatisfy` isDurationNear 0.100

        let
          (failed, succeeded) =
            partitionEithers
              $ map resultCandidateValue
              $ NE.toList
              $ resultDetailsCandidates rd

        succeeded `shouldMatchList` [A, A]
        length failed `shouldBe` 0

    it "is ResultIgnored if any candidates are ignored" $ do
      let
        ignoreB _ = \case
          Right (Candidate B) -> True
          _ -> False

      result <-
        experimentRunInternal
        $ setExperimentIgnore ignoreB
        $ setExperimentTry (pure C)
        $ setExperimentTry (pure B)
        $ newExperiment "test" (pure A)

      expectIgnored result $ \rd -> do
        resultDetailsExperimentName rd `shouldBe` "test"

        let control = resultDetailsControl rd
        resultControlValue control `shouldBe` A

        let
          (failed, succeeded) =
            partitionEithers
              $ map resultCandidateValue
              $ NE.toList
              $ resultDetailsCandidates rd

        succeeded `shouldMatchList` [B, C]
        length failed `shouldBe` 0

    it "is ResultMismatched if any candidates mismatched" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTry (pure B)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      expectMismatched result $ \rd -> do
        resultDetailsExperimentName rd `shouldBe` "test"

        let control = resultDetailsControl rd
        resultControlValue control `shouldBe` A

        let
          (failed, succeeded) =
            partitionEithers
              $ map resultCandidateValue
              $ NE.toList
              $ resultDetailsCandidates rd

        succeeded `shouldMatchList` [A, B]
        length failed `shouldBe` 0

    it "rescues exceptions in the Candidate branch" $ do
      result <-
        experimentRunInternal
        $ setExperimentTry (throwString "boom")
        $ newExperiment "test" (pure A)

      expectMismatched result $ \rd -> do
        resultDetailsExperimentName rd `shouldBe` "test"

        let
          control = resultDetailsControl rd
          mCandidateException =
            either fromException (const Nothing)
              $ resultCandidateValue
              $ NE.head
              $ resultDetailsCandidates rd

        resultControlValue control `shouldBe` A
        mCandidateException `shouldSatisfyMaybe` isStringException "boom"

    it "does not rescue exceptions in the Control branch" $ do
      experimentRunInternal (newExperiment "test" (throwString "boom"))
        `shouldThrowString` "boom"

    it "does not rescue exceptions in publishing" $ do
      experimentRunInternal
          (setExperimentPublish (\_ -> throwString "boom")
          $ setExperimentTry (pure B)
          $ newExperiment "test" (pure A)
          )
        `shouldThrowString` "boom"

    it "can be configured to rescue exceptions in publishing" $ do
      result <-
        experimentRunInternal
        $ setExperimentOnException (\_ -> pure ())
        $ setExperimentPublish (\_ -> throwString "boom")
        $ setExperimentTry (pure B)
        $ newExperiment "test" (pure A)

      expectMismatched result $ \_ -> pure ()

    it "supports implicitly named candidates" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTry (pure A)
        $ setExperimentTry (pure A)
        $ setExperimentTry (pure A)
        $ newExperiment "test" (pure A)

      expectMatched result $ \rd -> do
        resultControlName (resultDetailsControl rd) `shouldBe` "control"

        map resultCandidateName (NE.toList $ resultDetailsCandidates rd)
          `shouldMatchList` ["candidate", "candidate-1", "candidate-2"]

    it "supports explicitly named candidates" $ do
      result <-
        experimentRunInternal
        $ setExperimentCompare experimentCompareEq
        $ setExperimentTryNamed "who" (pure A)
        $ setExperimentTryNamed "what" (pure A)
        $ setExperimentTryNamed "when" (pure A)
        $ newExperiment "test" (pure A)

      expectMatched result $ \rd -> do
        resultControlName (resultDetailsControl rd) `shouldBe` "control"

        map resultCandidateName (NE.toList $ resultDetailsCandidates rd)
          `shouldMatchList` ["who", "what", "when"]

expectSkippedWith :: (Eq a, Show a) => Result c a b -> a -> IO ()
expectSkippedWith result a =
  expectSkipped result $ \(Control b) -> b `shouldBe` a

expectSkipped :: Result c a b -> (Control a -> IO ()) -> IO ()
expectSkipped result f = case result of
  ResultSkipped x -> f x
  _ -> expectationFailure "Expected result to be Skipped"

expectMatched :: Result c a b -> (ResultDetails c a b -> IO ()) -> IO ()
expectMatched result f = case result of
  ResultMatched rd -> f rd
  _ -> expectationFailure "Expected result to be Matched"

expectIgnored :: Result c a b -> (ResultDetails c a b -> IO ()) -> IO ()
expectIgnored result f = case result of
  ResultIgnored rd -> f rd
  _ -> expectationFailure "Expected result to be Ignored"

expectMismatched :: Result c a b -> (ResultDetails c a b -> IO ()) -> IO ()
expectMismatched result f = case result of
  ResultMismatched rd -> f rd
  _ -> expectationFailure "Expected result to be Mismatched"

isDurationNear :: Duration -> Duration -> Bool
isDurationNear x = isWithinOf x 0.050
