-- | Command-line interface for @aihc-dev fuzz@.
module Aihc.Dev.Fuzz.CLI
  ( Command (..),
    Options (..),
    Selection (..),
    commandParser,
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
  deriving (Eq, Show)

data Options = Options
  { optionsSelection :: Selection,
    optionsJobs :: Maybe Int,
    optionsTimeLimit :: Maybe NominalDiffTime
  }
  deriving (Eq, Show)

data Selection = Selection
  { selectionFilters :: [String],
    selectionExclusions :: [String]
  }
  deriving (Eq, Show)

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "list"
        ( info
            (List <$> selectionParser <**> helper)
            (progDesc "List available QuickCheck properties")
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
