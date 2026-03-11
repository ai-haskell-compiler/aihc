{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Scientist.Result
  ( Result(..)
  , resultValue
  , resultDetails
  , ResultDetails(..)
  , resultDetailsCandidate
  , ResultControl(..)
  , ResultCandidate(..)
  ) where

import Prelude

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Scientist.Control
import Scientist.Duration
import UnliftIO.Exception (SomeException)

data Result c a b
    = ResultSkipped (Control a)
    | ResultMatched (ResultDetails c a b)
    | ResultIgnored (ResultDetails c a b)
    | ResultMismatched (ResultDetails c a b)

resultValue :: Result c a b -> a
resultValue = \case
  ResultSkipped (Control a) -> a
  ResultMatched rd -> resultDetailsControlValue rd
  ResultIgnored rd -> resultDetailsControlValue rd
  ResultMismatched rd -> resultDetailsControlValue rd

resultDetails :: Result c a b -> Maybe (ResultDetails c a b)
resultDetails = \case
  ResultSkipped{} -> Nothing
  ResultMatched rd -> Just rd
  ResultIgnored rd -> Just rd
  ResultMismatched rd -> Just rd

data ResultDetails c a b = ResultDetails
  { resultDetailsExperimentName :: Text
  , resultDetailsExperimentContext :: Maybe c
  , resultDetailsControl :: ResultControl a
  , resultDetailsCandidates :: NonEmpty (ResultCandidate b)
  , resultDetailsExecutionOrder :: [Text]
  }

resultDetailsControlValue :: ResultDetails c a b -> a
resultDetailsControlValue = resultControlValue . resultDetailsControl

resultDetailsCandidate :: ResultDetails c a b -> ResultCandidate b
resultDetailsCandidate = NE.head . resultDetailsCandidates

data ResultControl a = ResultControl
  { resultControlName :: Text
  , resultControlValue :: a
  , resultControlDuration :: Duration
  }

data ResultCandidate a = ResultCandidate
  { resultCandidateName :: Text
  , resultCandidateValue :: Either SomeException a
  , resultCandidateDuration :: Duration
  }
