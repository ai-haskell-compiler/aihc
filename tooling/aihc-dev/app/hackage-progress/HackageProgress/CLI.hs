{-# LANGUAGE OverloadedStrings #-}

-- | CLI option parsing for hackage-progress using optparse-applicative.
module HackageProgress.CLI
  ( Options (..),
    optionsParser,
    toStackageOptions,
  )
where

import Data.List (nub)
import Options.Applicative qualified as OA
import StackageProgress.CLI qualified as StackageCLI

-- | Command-line options for hackage-progress.
data Options = Options
  { optParsers :: [StackageCLI.Parser],
    optJobs :: Maybe Int,
    optOffline :: Bool,
    optUpdateIndex :: Bool,
    optPrompt :: Bool,
    optPromptSeed :: Maybe Int,
    optPrintSucceeded :: Bool,
    optPrintFailedTable :: Bool,
    optGhcErrorsFile :: Maybe FilePath,
    optGhcErrorsLimit :: Int,
    optVerbose :: Bool
  }
  deriving (Eq, Show)

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> parserOption
    <*> OA.optional
      ( OA.option
          positiveIntReader
          ( OA.long "jobs"
              <> OA.metavar "N"
              <> OA.help "Number of packages to process concurrently (default: CPU cores)"
          )
      )
    <*> OA.switch
      ( OA.long "offline"
          <> OA.help "Use only cached index and packages, don't download"
      )
    <*> OA.switch
      ( OA.long "update-index"
          <> OA.help "Fetch and cache a fresh Hackage index before testing"
      )
    <*> OA.switch
      ( OA.long "prompt"
          <> OA.help "Generate a prompt for a random failing package"
      )
    <*> OA.optional
      ( OA.option
          OA.auto
          ( OA.long "prompt-seed"
              <> OA.metavar "N"
              <> OA.help "Seed for selecting random failing package"
          )
      )
    <*> OA.switch
      ( OA.long "print-succeeded"
          <> OA.help "Print list of succeeded packages"
      )
    <*> OA.switch
      ( OA.long "print-failed-table"
          <> OA.help "Print table of failed packages with sizes"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "ghc-errors-file"
              <> OA.metavar "PATH"
              <> OA.help "Write GHC errors to file"
          )
      )
    <*> OA.option
      positiveIntReader
      ( OA.long "ghc-errors-limit"
          <> OA.metavar "N"
          <> OA.value 100
          <> OA.showDefault
          <> OA.help "Maximum number of GHC errors to store"
      )
    <*> OA.switch
      ( OA.long "verbose"
          <> OA.help "Print verbose progress information"
      )

toStackageOptions :: Options -> StackageCLI.Options
toStackageOptions opts =
  StackageCLI.Options
    { StackageCLI.optSnapshot = "hackage",
      StackageCLI.optParsers = optParsers opts,
      StackageCLI.optJobs = optJobs opts,
      StackageCLI.optOffline = optOffline opts,
      StackageCLI.optPrompt = optPrompt opts,
      StackageCLI.optPromptSeed = optPromptSeed opts,
      StackageCLI.optPrintSucceeded = optPrintSucceeded opts,
      StackageCLI.optPrintFailedTable = optPrintFailedTable opts,
      StackageCLI.optGhcErrorsFile = optGhcErrorsFile opts,
      StackageCLI.optGhcErrorsLimit = optGhcErrorsLimit opts,
      StackageCLI.optVerbose = optVerbose opts
    }

parserOption :: OA.Parser [StackageCLI.Parser]
parserOption =
  OA.option
    parseParsers
    ( OA.long "parsers"
        <> OA.metavar "PARSERS"
        <> OA.value [StackageCLI.ParserAihc]
        <> OA.help "Comma-separated list of parsers: aihc,hse,ghc"
    )

parseParsers :: OA.ReadM [StackageCLI.Parser]
parseParsers = OA.eitherReader $ \raw -> do
  parsers <- mapM parseParser (splitComma raw)
  let uniq = nub parsers
  if null uniq
    then Left "--parsers cannot be empty"
    else Right uniq

parseParser :: String -> Either String StackageCLI.Parser
parseParser raw =
  case trim raw of
    "aihc" -> Right StackageCLI.ParserAihc
    "hse" -> Right StackageCLI.ParserHse
    "ghc" -> Right StackageCLI.ParserGhc
    other -> Left ("Unknown parser: " ++ other)

splitComma :: String -> [String]
splitComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitComma rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    dropWhileEnd p = reverse . dropWhile p . reverse

positiveIntReader :: OA.ReadM Int
positiveIntReader = OA.eitherReader $ \raw ->
  case reads raw of
    [(n, "")] | n > 0 -> Right n
    _ -> Left "must be a positive integer"
