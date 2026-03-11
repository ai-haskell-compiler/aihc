{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Scientist.Experiment.Run
  ( experimentRun
  , experimentRunInternal
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.Bifunctor (second)
import Data.Bitraversable (bimapM)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Scientist.Control
import Scientist.Duration
import Scientist.Experiment
import Scientist.NamedCandidate
import Scientist.Result
import Scientist.Result.Evaluate
import System.Random.Shuffle (shuffleM)
import UnliftIO.Exception (handleAny, throwString)

experimentRun :: MonadUnliftIO m => Experiment m c a b -> m a
experimentRun = fmap resultValue . experimentRunInternal

-- | 'experimentRun' but returning the full 'Result'
--
-- Used for testing.
--
experimentRunInternal
  :: MonadUnliftIO m => Experiment m c a b -> m (Result c a b)
experimentRunInternal ex = do
  enabled <- isExperimentEnabled ex

  let
    getName = \case
      Left{} -> "control"
      Right nc -> namedCandidateName nc

  case getExperimentTries ex of
    Just candidates | enabled -> do
      (controlResult, candidateResults, order) <- runRandomized
        control
        candidates
        runControl
        runCandidate
        getName

      let result = evaluateResult ex controlResult candidateResults order

      result <$ handleAny
        (getExperimentOnException ex)
        (getExperimentPublish ex result)

    _ -> ResultSkipped <$> control
  where control = getExperimentUse ex

isExperimentEnabled :: Applicative m => Experiment m c a b -> m Bool
isExperimentEnabled ex
  | not (getExperimentRunIf ex) = pure False
  | otherwise = getExperimentEnabled ex

runControl :: MonadIO m => m (Control a) -> m (ResultControl a)
runControl control = do
  (Control a, d) <- measureDuration control
  pure ResultControl
    { resultControlName = "control"
    , resultControlValue = a
    , resultControlDuration = d
    }

runCandidate :: MonadUnliftIO m => NamedCandidate m b -> m (ResultCandidate b)
runCandidate nc = do
  (b, d) <- measureDuration $ runNamedCandidate nc
  pure $ ResultCandidate
    { resultCandidateName = namedCandidateName nc
    , resultCandidateValue = b
    , resultCandidateDuration = d
    }

runRandomized
  :: MonadIO m
  => a
  -> NonEmpty b
  -> (a -> m a') -- ^ How to run the @a@
  -> (b -> m b') -- ^ How to run each @b@
  -> (Either a b -> Text)
  -- ^ How to identify each item in the reported order
  -> m (a', NonEmpty b', [Text])
runRandomized a bs runA runB toName = do
  inputs <- liftIO $ shuffleM $ Left a : map Right (NE.toList bs)
  outputs <- traverse (bimapM runA runB) inputs

  let
    order = map toName inputs
    partitioned = partitionEithers outputs

  case second NE.nonEmpty partitioned of
    ([a'], Just bs') -> pure (a', bs', order)
    _ ->
      -- Justification for this being "impossible":
      --
      -- - We cannot produce an a or b out of thin air
      -- - We were given an a and NonEmpty b
      -- - We cannot forget to use a without an unused warning
      -- - We cannot forget to use bs without an unused warning
      -- - We're doing no filtering anywhere
      --
      -- Therefore, there's no way to not get 1 Left and 1+ Rights here.
      --
      throwString
        $ "runRandomized did not produce 1 Left and 1+ Rights, but "
        <> show (length $ fst partitioned)
        <> " Left(s), and "
        <> show (length $ snd partitioned)
        <> " Rights(s)"
