{-# LANGUAGE DeriveGeneric #-}

module HackageTester.Model
  ( Outcome (..),
    FileResult (..),
    Summary (..),
    summarizeResults,
    shouldFailSummary,
    failureLabel,
  )
where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Outcome
  = OutcomeSuccess
  | OutcomeGhcError
  | OutcomeParseError
  | OutcomeRoundtripFail
  deriving (Eq, Show, Generic)

instance ToJSON Outcome

data FileResult = FileResult
  { filePath :: FilePath,
    outcome :: Outcome,
    cppDiagnostics :: [Text],
    outcomeDetail :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON FileResult

data Summary = Summary
  { totalFiles :: Int,
    ghcErrors :: Int,
    parseErrors :: Int,
    roundtripFails :: Int,
    successCount :: Int,
    failureCount :: Int,
    successRate :: Double
  }
  deriving (Eq, Show, Generic)

instance ToJSON Summary

summarizeResults :: [FileResult] -> Summary
summarizeResults results =
  let total = length results
      ghcErrs = length [() | r <- results, outcome r == OutcomeGhcError]
      parseErrs = length [() | r <- results, outcome r == OutcomeParseError]
      roundtripErrs = length [() | r <- results, outcome r == OutcomeRoundtripFail]
      successes = length [() | r <- results, outcome r == OutcomeSuccess]
      failures = total - successes
      rate =
        if total > 0
          then fromIntegral successes * 100.0 / fromIntegral total
          else 0.0
   in Summary
        { totalFiles = total,
          ghcErrors = ghcErrs,
          parseErrors = parseErrs,
          roundtripFails = roundtripErrs,
          successCount = successes,
          failureCount = failures,
          successRate = rate
        }

shouldFailSummary :: Summary -> Bool
shouldFailSummary summary =
  totalFiles summary == 0 || failureCount summary > 0

failureLabel :: Outcome -> Maybe String
failureLabel out =
  case out of
    OutcomeSuccess -> Nothing
    OutcomeGhcError -> Just "GHC_ERROR"
    OutcomeParseError -> Just "PARSE_ERROR"
    OutcomeRoundtripFail -> Just "ROUNDTRIP_FAIL"
