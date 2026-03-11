{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Scientist.Experiment
  ( Experiment

  -- * Construction
  , newExperiment

  -- * Modifiying values
  , setExperimentTry
  , setExperimentTryNamed
  , setExperimentEnabled
  , setExperimentOnException
  , setExperimentCompare
  , setExperimentContext
  , setExperimentIgnore
  , setExperimentRunIf
  , setExperimentPublish

  -- * Common modifying values
  , experimentCompareEq
  , experimentCompareOn
  , experimentCompareBy
  , experimentEnabledPercent

  -- * Accessing values
  , getExperimentName
  , getExperimentUse
  , getExperimentTries
  , getExperimentEnabled
  , getExperimentOnException
  , getExperimentCompare
  , getExperimentContext
  , getExperimentIgnore
  , getExperimentRunIf
  , getExperimentPublish
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Random (evalRandIO, getRandomR)
import Data.Function (on)
import Data.List.NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Scientist.Candidate
import Scientist.Control
import Scientist.NamedCandidate
import Scientist.Result
import UnliftIO.Exception (SomeException, throwIO)

data Experiment m c a b = Experiment
  { experimentName :: Text
  , experimentUse :: m (Control a)
  , experimentTries :: Maybe (NonEmpty (NamedCandidate m b))
  , experimentEnabled :: Maybe (m Bool)
  , experimentOnException :: Maybe (SomeException -> m ())
  , experimentCompare
      :: Maybe (Control a -> Either SomeException (Candidate b) -> Bool)
  , experimentContext :: Maybe c
  , experimentIgnore
      :: Maybe (Control a -> Either SomeException (Candidate b) -> Bool)
  , experimentRunIf :: Maybe Bool
  , experimentPublish :: Maybe (Result c a b -> m ())
  , experimentCandidateCount :: Int
  }

newExperiment :: Functor m => Text -> m a -> Experiment m c a b
newExperiment name f = Experiment
  { experimentName = name
  , experimentUse = Control <$> f
  , experimentTries = Nothing
  , experimentEnabled = Nothing
  , experimentOnException = Nothing
  , experimentCompare = Nothing
  , experimentContext = Nothing
  , experimentIgnore = Nothing
  , experimentRunIf = Nothing
  , experimentPublish = Nothing
  , experimentCandidateCount = 0
  }

-- | A new, candidate code path
--
-- If called multiple times, adds multiple candidate paths.
--
-- By default, there are no candidate paths and running the experiment will
-- return 'ResultSkipped'.
--
setExperimentTry
  :: Functor m => m b -> Experiment m c a b -> Experiment m c a b
setExperimentTry = setExperimentTryInternal Nothing

setExperimentTryNamed
  :: Functor m => Text -> m b -> Experiment m c a b -> Experiment m c a b
setExperimentTryNamed = setExperimentTryInternal . Just

setExperimentTryInternal
  :: Functor m => Maybe Text -> m b -> Experiment m c a b -> Experiment m c a b
setExperimentTryInternal mName f ex = ex
  { experimentTries = Just updated
  , experimentCandidateCount = updatedCount
  }
 where
  thisTry = pure $ namedCandidate thisName $ Candidate <$> f
  thisName = fromMaybe inferName mName
  inferName = case currentCount of
    0 -> "candidate"
    n -> pack $ "candidate-" <> show n

  current = experimentTries ex
  updated = maybe thisTry (<> thisTry) current

  currentCount = experimentCandidateCount ex
  updatedCount = currentCount + 1

-- | If the candidate paths should be executed
--
-- See 'experimentEnabledPercent' for an example.
--
-- By default, candidate paths are always run.
--
setExperimentEnabled :: m Bool -> Experiment m c a b -> Experiment m c a b
setExperimentEnabled f ex = ex { experimentEnabled = Just f }

-- | How to handle an exception evaluating or publishing
--
-- By default, the exception is re-thrown.
--
setExperimentOnException
  :: (SomeException -> m ()) -> Experiment m c a b -> Experiment m c a b
setExperimentOnException f ex = ex { experimentOnException = Just f }

-- | Decide if a given result is a match.
--
-- See 'experimentCompareEq' and 'experimentCompareBy'.
--
-- By default, all comparisons fail.
--
setExperimentCompare
  :: (Control a -> Either SomeException (Candidate b) -> Bool)
  -> Experiment m c a b
  -> Experiment m c a b
setExperimentCompare f ex = ex { experimentCompare = Just f }

-- | Decide if a given result should be ignored
--
-- By default, no results are ignored.
--
setExperimentIgnore
  :: (Control a -> Either SomeException (Candidate b) -> Bool)
  -> Experiment m c a b
  -> Experiment m c a b
setExperimentIgnore f ex = ex { experimentIgnore = Just f }

-- | Decide if the experiment should run at all
--
-- By default, experiments are run.
--
setExperimentRunIf :: Bool -> Experiment m c a b -> Experiment m c a b
setExperimentRunIf b ex = ex { experimentRunIf = Just b }

-- | How to publish results
--
-- By default, results are not published.
--
setExperimentPublish
  :: (Result c a b -> m ()) -> Experiment m c a b -> Experiment m c a b
setExperimentPublish f ex = ex { experimentPublish = Just f }

getExperimentName :: Experiment m c a b -> Text
getExperimentName = experimentName

getExperimentUse :: Experiment m c a b -> m (Control a)
getExperimentUse = experimentUse

getExperimentTries
  :: Experiment m c a b -> Maybe (NonEmpty (NamedCandidate m b))
getExperimentTries = experimentTries

getExperimentEnabled :: Applicative m => Experiment m c a b -> m Bool
getExperimentEnabled = fromMaybe (pure True) . experimentEnabled

getExperimentOnException
  :: MonadIO m => Experiment m c a b -> SomeException -> m ()
getExperimentOnException = fromMaybe throwIO . experimentOnException

getExperimentCompare
  :: Experiment m c a b
  -> (Control a -> Either SomeException (Candidate b) -> Bool)
getExperimentCompare = fromMaybe (\_ _ -> False) . experimentCompare

setExperimentContext :: c -> Experiment m c a b -> Experiment m c a b
setExperimentContext x ex = ex { experimentContext = Just x }

getExperimentContext :: Experiment m c a b -> Maybe c
getExperimentContext = experimentContext

getExperimentIgnore
  :: Experiment m c a b
  -> (Control a -> Either SomeException (Candidate b) -> Bool)
getExperimentIgnore = fromMaybe (\_ _ -> False) . experimentIgnore

getExperimentRunIf :: Experiment m c a b -> Bool
getExperimentRunIf = fromMaybe True . experimentRunIf

getExperimentPublish
  :: Applicative m => Experiment m c a b -> (Result c a b -> m ())
getExperimentPublish = fromMaybe (const $ pure ()) . experimentPublish

-- | Compare non-exception candidates with the control by '(==)'
--
-- Exception candidates fail comparison.
--
experimentCompareEq
  :: Eq a => Control a -> Either SomeException (Candidate a) -> Bool
experimentCompareEq = experimentCompareBy (==)

-- | Compare by equality on some function
--
-- Exception candidates fail comparison.
--
experimentCompareOn
  :: Eq b => (a -> b) -> Control a -> Either SomeException (Candidate a) -> Bool
experimentCompareOn f = experimentCompareBy ((==) `on` f)

-- | Compare by some function
--
-- Exception candidates fail comparison.
--
experimentCompareBy
  :: (a -> b -> Bool) -> Control a -> Either SomeException (Candidate b) -> Bool
experimentCompareBy f (Control a) = \case
  Left _ -> False
  Right (Candidate b) -> f a b

-- | Enable the experiment in the given percentage of runs
experimentEnabledPercent :: MonadIO m => Int -> m Bool
experimentEnabledPercent n
  | n <= 0 = pure False
  | n >= 100 = pure True
  | otherwise = liftIO $ evalRandIO $ (<= n) <$> getRandomR (0, 100)
