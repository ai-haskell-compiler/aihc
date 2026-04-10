{-# LANGUAGE LambdaCase #-}

-- | Command-line interface for aihc-parser-bench.
module Aihc.Parser.Bench.CLI
  ( Options (..),
    Command (..),
    GenerateOptions (..),
    BenchOptions (..),
    ParserChoice (..),
    OutputFormat (..),
    FilterOptions (..),
    parseOptionsIO,
  )
where

import Options.Applicative

-- | Which parser to use for benchmarking or filtering.
data ParserChoice
  = ParserAihc
  | ParserHse
  | ParserGhc
  deriving (Eq, Show)

-- | Output format for benchmark results.
data OutputFormat
  = FormatHuman
  | FormatJson
  | FormatCsv
  deriving (Eq, Show)

-- | Filter options for tarball generation.
data FilterOptions = FilterOptions
  { filterAihc :: !Bool,
    filterHse :: !Bool,
    filterGhc :: !Bool
  }
  deriving (Eq, Show)

-- | Options for the generate subcommand.
data GenerateOptions = GenerateOptions
  { genSnapshot :: !String,
    genOutput :: !(Maybe FilePath),
    genFilters :: !FilterOptions,
    genCacheDir :: !(Maybe FilePath),
    genOffline :: !Bool,
    genVerbose :: !Bool,
    genDryRun :: !Bool
  }
  deriving (Eq, Show)

-- | Options for the bench subcommand.
data BenchOptions = BenchOptions
  { benchTarball :: !FilePath,
    benchParser :: !ParserChoice,
    benchLexerOnly :: !Bool,
    benchWarmup :: !Int,
    benchIterations :: !Int,
    benchOutput :: !OutputFormat,
    benchGcStats :: !Bool
  }
  deriving (Eq, Show)

-- | Top-level command.
data Command
  = CmdGenerate !GenerateOptions
  | CmdBench !BenchOptions
  deriving (Show)

-- | Top-level options.
newtype Options = Options
  { optCommand :: Command
  }
  deriving (Show)

-- | Parse command-line options.
parseOptionsIO :: IO Options
parseOptionsIO = execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Generate stackage tarballs and benchmark Haskell parsers"
            <> header "aihc-parser-bench - Haskell parser benchmarking tool"
        )

optionsParser :: Parser Options
optionsParser = Options <$> commandParser

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "generate"
        ( info
            (CmdGenerate <$> generateParser)
            (progDesc "Generate a tarball of Haskell sources from Stackage")
        )
        <> command
          "bench"
          ( info
              (CmdBench <$> benchOptionsParser)
              (progDesc "Benchmark parsing performance on a tarball")
          )
    )

generateParser :: Parser GenerateOptions
generateParser =
  GenerateOptions
    <$> strOption
      ( long "snapshot"
          <> metavar "NAME"
          <> value "lts-24.36"
          <> showDefault
          <> help "Stackage snapshot name"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Output tarball path (default: stackage-<snapshot>.tar.gz)"
          )
      )
    <*> filterOptionsParser
    <*> optional
      ( strOption
          ( long "cache-dir"
              <> metavar "DIR"
              <> help "Cache directory for downloads"
          )
      )
    <*> switch
      ( long "offline"
          <> help "Only use cached packages"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Print progress information"
      )
    <*> switch
      ( long "dry-run"
          <> help "List packages without downloading or creating tarball"
      )

filterOptionsParser :: Parser FilterOptions
filterOptionsParser =
  FilterOptions
    <$> switch
      ( long "filter-aihc"
          <> help "Only include packages parseable by aihc-parser"
      )
    <*> switch
      ( long "filter-hse"
          <> help "Only include packages parseable by haskell-src-exts"
      )
    <*> switch
      ( long "filter-ghc"
          <> help "Only include packages parseable by ghc-lib-parser"
      )

benchOptionsParser :: Parser BenchOptions
benchOptionsParser =
  BenchOptions
    <$> strArgument
      ( metavar "TARBALL"
          <> help "Tarball of Haskell sources to benchmark"
      )
    <*> option
      parseParserChoice
      ( long "parser"
          <> short 'p'
          <> metavar "PARSER"
          <> value ParserAihc
          <> showDefaultWith
            ( \case
                ParserAihc -> "aihc"
                ParserHse -> "hse"
                ParserGhc -> "ghc"
            )
          <> help "Parser to use: aihc, hse, ghc"
      )
    <*> switch
      ( long "lexer-only"
          <> help "Lex but don't parse (aihc only)"
      )
    <*> option
      auto
      ( long "warmup"
          <> metavar "N"
          <> value 1
          <> showDefault
          <> help "Number of warmup iterations"
      )
    <*> option
      auto
      ( long "iterations"
          <> metavar "N"
          <> value 3
          <> showDefault
          <> help "Number of benchmark iterations"
      )
    <*> option
      parseOutputFormat
      ( long "output"
          <> short 'o'
          <> metavar "FORMAT"
          <> value FormatHuman
          <> help "Output format: human, json, csv (default: human)"
      )
    <*> switch
      ( long "gc-stats"
          <> help "Include GC statistics (requires +RTS -T)"
      )

parseParserChoice :: ReadM ParserChoice
parseParserChoice = eitherReader $ \s ->
  case s of
    "aihc" -> Right ParserAihc
    "hse" -> Right ParserHse
    "ghc" -> Right ParserGhc
    _ -> Left $ "Unknown parser: " ++ s ++ ". Expected: aihc, hse, or ghc"

parseOutputFormat :: ReadM OutputFormat
parseOutputFormat = eitherReader $ \s ->
  case s of
    "human" -> Right FormatHuman
    "json" -> Right FormatJson
    "csv" -> Right FormatCsv
    _ -> Left $ "Unknown format: " ++ s ++ ". Expected: human, json, or csv"
