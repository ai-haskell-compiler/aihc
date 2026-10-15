-- | Command-line interface for @aihc-dev fuzz@.
module Aihc.Dev.Fuzz.CLI
  ( BugsOptions (..),
    Command (..),
    Options (..),
    ReplayOptions (..),
    Selection (..),
    commandParser,
    defaultRepository,
    parseDuration,
  )
where

import Data.Char (isDigit)
import Data.Time.Clock (NominalDiffTime)
import Options.Applicative
import Text.Read (readMaybe)

data Command
  = List Selection
  | Run Options
  | Bugs BugsOptions
  | Replay ReplayOptions
  deriving (Eq, Show)

data Options = Options
  { optionsSelection :: Selection,
    optionsJobs :: Maybe Int,
    optionsTimeLimit :: Maybe NominalDiffTime,
    optionsRepository :: String,
    optionsReport :: Bool,
    optionsBugFile :: Maybe FilePath,
    optionsFollow :: Bool
  }
  deriving (Eq, Show)

data Selection = Selection
  { selectionFilters :: [String],
    selectionExclusions :: [String]
  }
  deriving (Eq, Show)

data BugsOptions = BugsOptions
  { bugsForget :: [String],
    bugsClear :: Bool,
    bugsBugFile :: Maybe FilePath
  }
  deriving (Eq, Show)

data ReplayOptions = ReplayOptions
  { replayProperty :: String,
    replayArgument :: String
  }
  deriving (Eq, Show)

-- | The repository that receives the tracking issue.
defaultRepository :: String
defaultRepository = "ai-haskell-compiler/aihc"

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "list"
        ( info
            (List <$> selectionParser <**> helper)
            (progDesc "List available Hedgehog properties")
        )
        <> command
          "bugs"
          ( info
              (Bugs <$> bugsParser <**> helper)
              (progDesc "List, forget, or clear the known bugs")
          )
        <> command
          "replay"
          ( info
              (Replay <$> replayParser <**> helper)
              (progDesc "Run one recorded failure again")
          )
    )
    <|> (Run <$> optionsParser)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> selectionParser
    <*> optional
      ( option
          positiveInt
          ( long "jobs"
              <> short 'j'
              <> metavar "N"
              <> help "Number of properties to run in parallel (default: RTS capabilities)"
          )
      )
    <*> optional
      ( option
          durationReader
          ( long "time-limit"
              <> metavar "DURATION"
              <> help "Stop after a duration such as 30s, 10m, or 2h"
          )
      )
    <*> strOption
      ( long "repo"
          <> metavar "OWNER/NAME"
          <> value defaultRepository
          <> showDefault
          <> help "GitHub repository that receives the tracking issue"
      )
    <*> flag
      True
      False
      ( long "no-report"
          <> help "Record bugs locally and do not report them on GitHub"
      )
    <*> bugFileParser
    <*> switch
      ( long "follow"
          <> help "Build each new commit of the selected branch in the background and restart with it"
      )

bugsParser :: Parser BugsOptions
bugsParser =
  BugsOptions
    <$> many
      ( strOption
          ( long "forget"
              <> metavar "BUG-ID"
              <> help "Remove the bug with this id from the store (repeatable)"
          )
      )
    <*> switch
      ( long "clear"
          <> help "Remove every known bug from the store"
      )
    <*> bugFileParser

replayParser :: Parser ReplayOptions
replayParser =
  ReplayOptions
    <$> strArgument
      ( metavar "PROPERTY"
          <> help "Component-qualified property name, for example \"aihc-tc.zonking idempotent\""
      )
    <*> strArgument
      ( metavar "REPLAY"
          <> help "Shrink path and seed, for example \"3:b2 Seed 1 3\", or only the seed"
      )

bugFileParser :: Parser (Maybe FilePath)
bugFileParser =
  optional
    ( strOption
        ( long "bug-file"
            <> metavar "PATH"
            <> help "Bug store file (default: $XDG_CACHE_HOME/aihc/fuzz/bugs.json)"
        )
    )

selectionParser :: Parser Selection
selectionParser =
  Selection
    <$> many
      ( strOption
          ( long "filter"
              <> short 'f'
              <> metavar "TEXT"
              <> help "Run properties whose component-qualified name contains TEXT (repeatable)"
          )
      )
    <*> many
      ( strOption
          ( long "exclude"
              <> short 'x'
              <> metavar "TEXT"
              <> help "Skip properties whose component-qualified name contains TEXT (repeatable)"
          )
      )

positiveInt :: ReadM Int
positiveInt = eitherReader $ \raw ->
  case readMaybe raw of
    Just parsed | parsed > 0 -> Right parsed
    _ -> Left "expected a positive integer"

durationReader :: ReadM NominalDiffTime
durationReader = eitherReader parseDuration

parseDuration :: String -> Either String NominalDiffTime
parseDuration raw =
  case span (\char -> isDigit char || char == '.') raw of
    (number, suffix)
      | Just durationValue <- readMaybe number,
        durationValue > (0 :: Double),
        Just multiplier <- durationMultiplier suffix ->
          Right (realToFrac (durationValue * multiplier))
    _ -> Left "expected a positive duration with suffix ms, s, m, or h (for example 30s)"

durationMultiplier :: String -> Maybe Double
durationMultiplier suffix =
  case suffix of
    "ms" -> Just 0.001
    "s" -> Just 1
    "m" -> Just 60
    "h" -> Just 3600
    _ -> Nothing
