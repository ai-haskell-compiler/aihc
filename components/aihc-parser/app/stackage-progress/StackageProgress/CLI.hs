{-# LANGUAGE OverloadedStrings #-}

-- | CLI option parsing for stackage-progress using optparse-applicative.
module StackageProgress.CLI
  ( Options (..),
    Check (..),
    parseOptionsIO,
    parseOptionsPure,
    summaryOptionsFromOptions,
  )
where

import Data.List (nub)
import qualified Options.Applicative as OA
import StackageProgress.Summary (SummaryOptions (..))

-- | Type of check to perform on each file.
data Check
  = CheckParse
  | CheckRoundtripGhc
  | CheckSourceSpan
  | CheckHse
  | CheckGhc
  deriving (Eq, Show)

-- | Command-line options for stackage-progress.
data Options = Options
  { optSnapshot :: String,
    optChecks :: [Check],
    optJobs :: Maybe Int,
    optOffline :: Bool,
    optPrompt :: Bool,
    optPromptSeed :: Maybe Int,
    optPrintSucceeded :: Bool,
    optPrintFailedTable :: Bool,
    optSanityCheck :: Bool,
    optGhcErrorsFile :: Maybe FilePath,
    optGhcErrorsLimit :: Int
  }
  deriving (Eq, Show)

-- | Parse options from command line.
parseOptionsIO :: IO Options
parseOptionsIO = OA.execParser parserInfo

-- | Parse options from a list of arguments (for testing).
parseOptionsPure :: [String] -> Either String Options
parseOptionsPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success opts -> Right opts
    OA.Failure failure ->
      let (msg, _) = OA.renderFailure failure "stackage-progress"
       in Left msg
    OA.CompletionInvoked _ ->
      Left "shell completion requested"

parserInfo :: OA.ParserInfo Options
parserInfo =
  OA.info
    (optionsParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Test parser on Stackage snapshot packages"
        <> OA.header "stackage-progress - Stackage package testing"
    )

optionsParser :: OA.Parser Options
optionsParser =
  postProcessOptions
    <$> ( Options
            <$> OA.strOption
              ( OA.long "snapshot"
                  <> OA.metavar "SNAPSHOT"
                  <> OA.value "lts-24.33"
                  <> OA.showDefault
                  <> OA.help "Stackage snapshot to test"
              )
            <*> checksOption
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
                  <> OA.help "Use only cached packages, don't download"
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
            <*> OA.switch
              ( OA.long "sanity-check"
                  <> OA.help "Also run HSE and GHC checks for comparison"
              )
            <*> OA.optional
              ( OA.strOption
                  ( OA.long "ghc-errors-file"
                      <> OA.metavar "PATH"
                      <> OA.help "Write GHC errors to file"
                  )
              )
            <*> OA.option
              OA.auto
              ( OA.long "ghc-errors-limit"
                  <> OA.metavar "N"
                  <> OA.value 100
                  <> OA.showDefault
                  <> OA.help "Maximum number of GHC errors to store"
              )
        )

checksOption :: OA.Parser [Check]
checksOption =
  OA.option
    parseChecks
    ( OA.long "checks"
        <> OA.metavar "CHECKS"
        <> OA.value [CheckParse]
        <> OA.help "Comma-separated list of checks: parse,roundtrip-ghc,source-span"
    )

parseChecks :: OA.ReadM [Check]
parseChecks = OA.eitherReader $ \raw -> do
  checks <- mapM parseCheck (splitComma raw)
  let uniq = nub checks
  if null uniq
    then Left "--checks cannot be empty"
    else Right uniq

parseCheck :: String -> Either String Check
parseCheck raw =
  case trim raw of
    "parse" -> Right CheckParse
    "roundtrip-ghc" -> Right CheckRoundtripGhc
    "source-span" -> Right CheckSourceSpan
    other -> Left ("Unknown check: " ++ other)

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

-- | Post-process options to handle sanity-check flag.
postProcessOptions :: Options -> Options
postProcessOptions opts
  | optSanityCheck opts =
      opts {optChecks = nub (optChecks opts ++ [CheckHse, CheckGhc])}
  | otherwise = opts

-- | Convert Options to SummaryOptions.
summaryOptionsFromOptions :: Options -> SummaryOptions
summaryOptionsFromOptions opts =
  SummaryOptions
    { summaryKeepSucceeded = optPrintSucceeded opts,
      summaryKeepFailedPackages = optPrintFailedTable opts,
      summaryGhcErrorLimit =
        case optGhcErrorsFile opts of
          Just _ -> optGhcErrorsLimit opts
          Nothing -> 0
    }
