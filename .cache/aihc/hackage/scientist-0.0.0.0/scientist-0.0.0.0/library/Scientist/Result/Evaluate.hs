module Scientist.Result.Evaluate
  ( evaluateResult
  ) where

import Prelude

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Scientist.Candidate
import Scientist.Control
import Scientist.Experiment
import Scientist.Result

evaluateResult
  :: Experiment m c a b
  -> ResultControl a
  -> NonEmpty (ResultCandidate b)
  -> [Text]
  -> Result c a b
evaluateResult ex control candidates order
  | any (ignore control) candidates = ResultIgnored details
  | all (match control) candidates = ResultMatched details
  | otherwise = ResultMismatched details
 where
  ignore a b = getExperimentIgnore
    ex
    (Control $ resultControlValue a)
    (Candidate <$> resultCandidateValue b)

  match a b = getExperimentCompare
    ex
    (Control $ resultControlValue a)
    (Candidate <$> resultCandidateValue b)

  details = ResultDetails
    { resultDetailsExperimentName = getExperimentName ex
    , resultDetailsExperimentContext = getExperimentContext ex
    , resultDetailsControl = control
    , resultDetailsCandidates = candidates
    , resultDetailsExecutionOrder = order
    }
