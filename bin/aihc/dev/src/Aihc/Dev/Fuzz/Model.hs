{-# LANGUAGE NamedFieldPuns #-}

-- | Pure dashboard data: snapshots, rows, rates, and number formats.
module Aihc.Dev.Fuzz.Model
  ( Announcement (..),
    BugRow (..),
    Event (..),
    Outcome (..),
    PropertyRow (..),
    ReporterStatus (..),
    Snapshot (..),
    bugRows,
    bugStatus,
    describeReporter,
    formatCount,
    formatElapsed,
    formatInteger,
    propertyRows,
    updateRates,
  )
where

import Aihc.Dev.Fuzz.Bugs (Bug (..), IssueRef (..))
import Aihc.Dev.Fuzz.Registry (FuzzProperty (..), fuzzPropertyId)
import Control.Exception (SomeException)
import Data.List (intercalate, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Text.Printf (printf)

-- | What the issue reporter does at the moment.
data ReporterStatus
  = -- | No bug waits for a report.
    ReporterIdle
  | -- | Reports wait because @gh@ is not usable, for the given reason.
    ReporterBlocked String
  | -- | The bug with the given id is being reported.
    ReporterSending String
  | -- | The last report failed with the given error.
    ReporterFailed String
  deriving (Eq, Show)

-- | A consistent view of the runner state at one time.
data Snapshot = Snapshot
  { snapshotTime :: UTCTime,
    snapshotActive :: [(Int, String)],
    snapshotPropertyCases :: Map String Int,
    snapshotCases :: Int,
    snapshotBatches :: Int,
    snapshotBugs :: [Bug],
    snapshotNewBugs :: Set String,
    snapshotReporter :: ReporterStatus
  }

data Event
  = Tick Snapshot
  | TimeLimitEvent
  | Crashed SomeException

data Outcome
  = TimeLimitReached
  | UserQuit
  | WorkerCrashed SomeException

-- | A message for the log in non-interactive mode.
data Announcement
  = -- | A failure was recorded. The flag is true for a new bug.
    FoundBug Bug Bool
  | ReportedBug Bug
  | ReporterChanged ReporterStatus

data PropertyRow = PropertyRow
  { rowComponent :: String,
    rowName :: String,
    rowCases :: Int,
    rowRate :: Double,
    rowWorkers :: Int,
    rowBugs :: Int
  }
  deriving (Eq, Show)

data BugRow = BugRow
  { bugRowId :: String,
    bugRowProperty :: String,
    bugRowHits :: Int,
    bugRowFirstSeen :: UTCTime,
    bugRowNew :: Bool,
    bugRowIssue :: Maybe IssueRef,
    bugRowStatus :: String
  }
  deriving (Eq, Show)

-- | One row for each selected property, sorted by component and name.
propertyRows :: [FuzzProperty] -> Snapshot -> Map String Double -> [PropertyRow]
propertyRows properties Snapshot {snapshotActive, snapshotPropertyCases, snapshotBugs} rates =
  sortOn (\row -> (rowComponent row, rowName row)) (map toRow properties)
  where
    toRow fuzzProperty =
      let propertyId = fuzzPropertyId fuzzProperty
       in PropertyRow
            { rowComponent = fuzzPropertyComponent fuzzProperty,
              rowName = fuzzPropertyName fuzzProperty,
              rowCases = Map.findWithDefault 0 propertyId snapshotPropertyCases,
              rowRate = Map.findWithDefault 0 propertyId rates,
              rowWorkers = length [() | (_, active) <- snapshotActive, active == propertyId],
              rowBugs = length [() | bug <- snapshotBugs, bugProperty bug == propertyId]
            }

-- | One row for each known bug, newest first.
bugRows :: Snapshot -> [BugRow]
bugRows Snapshot {snapshotBugs, snapshotNewBugs, snapshotReporter} =
  map toRow (reverse snapshotBugs)
  where
    toRow bug =
      BugRow
        { bugRowId = bugId bug,
          bugRowProperty = bugProperty bug,
          bugRowHits = bugHits bug,
          bugRowFirstSeen = bugFirstSeen bug,
          bugRowNew = bugId bug `Set.member` snapshotNewBugs,
          bugRowIssue = bugIssue bug,
          bugRowStatus = bugStatus snapshotReporter bug
        }

-- | Describe the report state of one bug.
bugStatus :: ReporterStatus -> Bug -> String
bugStatus reporter bug =
  case bugIssue bug of
    Just issue -> "#" <> show (issueNumber issue)
    Nothing ->
      case reporter of
        ReporterSending identifier | identifier == bugId bug -> "reporting…"
        ReporterBlocked reason -> "pending · " <> reason
        ReporterFailed reason -> "pending · " <> reason
        _ -> "pending"

describeReporter :: ReporterStatus -> String
describeReporter status =
  case status of
    ReporterIdle -> "idle"
    ReporterBlocked reason -> "blocked, " <> reason
    ReporterSending identifier -> "reporting " <> take 12 identifier
    ReporterFailed reason -> "failed, " <> reason

-- | Blend the latest cases-per-second sample into the smoothed rates.
-- A property that is not in the previous sample starts from zero cases.
updateRates :: Double -> Map String Int -> Map String Int -> Map String Double -> Map String Double
updateRates seconds previous current rates
  | seconds <= 0 = rates
  | otherwise = Map.mapWithKey blend current
  where
    blend name count =
      let instant = fromIntegral (count - Map.findWithDefault 0 name previous) / seconds
       in case Map.lookup name rates of
            Nothing -> instant
            Just old -> old * 0.8 + instant * 0.2

formatElapsed :: NominalDiffTime -> String
formatElapsed elapsed =
  let totalSeconds = max 0 (floor elapsed :: Int)
      (hours, afterHours) = totalSeconds `divMod` 3600
      (minutes, seconds) = afterHours `divMod` 60
   in printf "%02d:%02d:%02d" hours minutes seconds

-- | Use decimal SI prefixes for cumulative test counts.
formatCount :: Int -> String
formatCount value
  | value < 1000 = show value
  | otherwise = scale (fromIntegral value :: Double) ["K", "M", "G", "T", "P", "E"]
  where
    scale :: Double -> [String] -> String
    scale number (suffix : rest)
      | number >= 999950 && not (null rest) = scale (number / 1000) rest
      | otherwise = printf "%.1f%s" (number / 1000) suffix
    scale number [] = printf "%.0f" number

-- | Group the digits of a count in threes.
formatInteger :: Int -> String
formatInteger value =
  reverse . intercalate "," . chunksOfThree . reverse $ show value
  where
    chunksOfThree [] = []
    chunksOfThree input = take 3 input : chunksOfThree (drop 3 input)
