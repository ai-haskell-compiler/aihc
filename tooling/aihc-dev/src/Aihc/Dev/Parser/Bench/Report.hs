{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Markdown report generation for relative Stackage parser and CPP benchmarks.
module Aihc.Dev.Parser.Bench.Report
  ( runReport,
  )
where

import Aihc.Dev.Parser.Bench.CLI
  ( FilterOptions (..),
    GenerateOptions (..),
    ReportOptions (..),
  )
import Aihc.Dev.Parser.Bench.Parsers
  ( ParseResult (..),
    parseWithAihcExtsWithCpp,
    parseWithGhcExtsWithCpp,
    prepareSourceAndExtensionsWithCpp,
    runCppWithIncludes,
  )
import Aihc.Dev.Parser.Bench.Tarball
  ( TarballEntry (..),
    generateTarballEntries,
    isHaskellEntry,
    isIncludeEntry,
  )
import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, bracket, evaluate, try)
import Control.Monad (forM_, unless, void)
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
  ( createDirectoryIfMissing,
    getTemporaryDirectory,
    removePathForcibly,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

data Corpus = Corpus
  { corpusEntries :: ![TarballEntry],
    corpusHaskellEntries :: ![TarballEntry],
    corpusIncludeMap :: !(Map.Map FilePath Text),
    corpusPackageCount :: !Int,
    corpusFileCount :: !Int,
    corpusByteCount :: !Integer
  }

data Timed a = Timed
  { timedResult :: !a,
    timedNanos :: !Integer
  }

data ToolResult = ToolResult
  { toolName :: !String,
    toolNanos :: !Integer
  }

runReport :: ReportOptions -> IO ()
runReport opts@ReportOptions {reportOutput} = do
  corpus <- loadCorpus opts
  unless (corpusFileCount corpus > 0) $
    fail "benchmark corpus is empty"

  hPutStrLn stderr "Preprocessing parser corpus with aihc-cpp..."
  preprocessed <- preprocessCorpus corpus

  hPutStrLn stderr "Benchmarking parsers..."
  ghcParser <- benchmarkParser "GHC (`ghc-lib-parser`)" parseGhc preprocessed
  aihcParser <- benchmarkParser "AIHC" parseAihc preprocessed

  hPutStrLn stderr "Staging corpus for external preprocessors..."
  (cpphsResult, clangResult) <-
    withStagedCorpus (corpusEntries corpus) $ \root -> do
      hPutStrLn stderr "Benchmarking cpphs..."
      cpphsResult <- benchmarkExternalCpp "cpphs" (runCpphs root) (corpusHaskellEntries corpus)
      hPutStrLn stderr "Benchmarking clang -E..."
      clangResult <- benchmarkExternalCpp "clang -E" (runClang root) (corpusHaskellEntries corpus)
      pure (cpphsResult, clangResult)

  hPutStrLn stderr "Benchmarking aihc-cpp..."
  aihcCpp <- benchmarkAihcCpp corpus

  commit <- currentCommit
  let markdown =
        renderReport
          opts
          corpus
          commit
          [ghcParser, aihcParser]
          [clangResult, cpphsResult, aihcCpp]
  TIO.writeFile reportOutput markdown
  hPutStrLn stderr ("Wrote " ++ reportOutput)

loadCorpus :: ReportOptions -> IO Corpus
loadCorpus ReportOptions {reportSnapshot, reportOffline} = do
  let genOpts =
        GenerateOptions
          { genSnapshot = reportSnapshot,
            genOutput = Nothing,
            genFilters =
              FilterOptions
                { filterAihc = False,
                  filterHse = False,
                  filterGhc = False
                },
            genCacheDir = Nothing,
            genOffline = reportOffline,
            genVerbose = False,
            genDryRun = True,
            genPreprocess = False
          }
  result <- generateTarballEntries genOpts
  case result of
    Left err -> fail err
    Right (entries, _summary) -> do
      let hsEntries = filter isHaskellEntry entries
          includeMap = Map.fromList [(entryFilePath e, entryContents e) | e <- filter isIncludeEntry entries]
          packages = nub [entryPackage e | e <- hsEntries]
      pure
        Corpus
          { corpusEntries = entries,
            corpusHaskellEntries = hsEntries,
            corpusIncludeMap = includeMap,
            corpusPackageCount = length packages,
            corpusFileCount = length hsEntries,
            corpusByteCount = sum (map (fromIntegral . entryByteSize) hsEntries)
          }

preprocessCorpus :: Corpus -> IO [TarballEntry]
preprocessCorpus Corpus {corpusHaskellEntries, corpusIncludeMap} =
  mapM preprocessEntry corpusHaskellEntries
  where
    preprocessEntry entry = do
      let (source, _) =
            prepareSourceAndExtensionsWithCpp
              False
              corpusIncludeMap
              (entryFilePath entry)
              (entryExtensions entry)
              (entryCppOptions entry)
              (entryLanguage entry)
              (entryDependencies entry)
              (entryContents entry)
      evaluate (source `deepseq` entry {entryContents = source, entryByteSize = BS.length (TE.encodeUtf8 source), entryCppOptions = []})

benchmarkParser :: String -> (TarballEntry -> ParseResult) -> [TarballEntry] -> IO ToolResult
benchmarkParser name parser entries = do
  timed <- timeAction $ evaluateResults (map parser entries)
  pure ToolResult {toolName = name, toolNanos = timedNanos timed}

evaluateResults :: [ParseResult] -> IO ()
evaluateResults results =
  evaluate (results `deepseq` ())

parseAihc :: TarballEntry -> ParseResult
parseAihc entry =
  parseWithAihcExtsWithCpp
    True
    Map.empty
    (entryFilePath entry)
    (entryExtensions entry)
    []
    (entryLanguage entry)
    (entryDependencies entry)
    (entryContents entry)

parseGhc :: TarballEntry -> ParseResult
parseGhc entry =
  parseWithGhcExtsWithCpp
    True
    Map.empty
    (entryFilePath entry)
    (entryExtensions entry)
    []
    (entryLanguage entry)
    (entryDependencies entry)
    (entryContents entry)

benchmarkAihcCpp :: Corpus -> IO ToolResult
benchmarkAihcCpp Corpus {corpusHaskellEntries, corpusIncludeMap} = do
  timed <-
    timeAction $
      forM_ corpusHaskellEntries $ \entry -> do
        let output =
              runCppWithIncludes
                corpusIncludeMap
                (entryFilePath entry)
                (entryCppOptions entry)
                (entryDependencies entry)
                (entryContents entry)
        evaluate (output `deepseq` ())
  pure ToolResult {toolName = "aihc-cpp", toolNanos = timedNanos timed}

benchmarkExternalCpp :: String -> (TarballEntry -> IO ()) -> [TarballEntry] -> IO ToolResult
benchmarkExternalCpp name runOne entries = do
  timed <- timeAction (mapM_ runOne entries)
  pure ToolResult {toolName = name, toolNanos = timedNanos timed}

runCpphs :: FilePath -> TarballEntry -> IO ()
runCpphs root entry = do
  let path = root </> entryFilePath entry
      args = ["--noline", "--strip", "--nowarn"] ++ entryCppOptions entry ++ [path]
  runExternal "cpphs" args

runClang :: FilePath -> TarballEntry -> IO ()
runClang root entry = do
  let path = root </> entryFilePath entry
      args = ["-E", "-P", "-x", "assembler-with-cpp"] ++ entryCppOptions entry ++ [path]
  runExternal "clang" args

runExternal :: FilePath -> [String] -> IO ()
runExternal exe args = do
  result <- try (readProcessWithExitCode exe args "") :: IO (Either SomeException (ExitCode, String, String))
  case result of
    Left err -> fail (exe ++ " failed to start: " ++ show err)
    Right (ExitSuccess, out, err) ->
      void (evaluate (length out + length err))
    Right (ExitFailure code, _, err) ->
      fail (exe ++ " failed with exit code " ++ show code ++ ": " ++ take 500 err)

withStagedCorpus :: [TarballEntry] -> (FilePath -> IO a) -> IO a
withStagedCorpus entries action = do
  tmp <- getTemporaryDirectory
  stamp <- getMonotonicTimeNSec
  let root = tmp </> ("aihc-bench-corpus-" ++ show stamp)
  bracket
    (createDirectoryIfMissing True root >> pure root)
    removePathForcibly
    $ \dir -> do
      forM_ entries $ \entry -> do
        let path = dir </> entryFilePath entry
        createDirectoryIfMissing True (takeDirectory path)
        TIO.writeFile path (entryContents entry)
      action dir

timeAction :: IO a -> IO (Timed a)
timeAction action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure Timed {timedResult = result, timedNanos = fromIntegral (end - start)}

currentCommit :: IO String
currentCommit = do
  result <- try (readProcessWithExitCode "git" ["rev-parse", "HEAD"] "") :: IO (Either SomeException (ExitCode, String, String))
  pure $ case result of
    Right (ExitSuccess, out, _) -> trim out
    _ -> "unknown"

renderReport :: ReportOptions -> Corpus -> String -> [ToolResult] -> [ToolResult] -> Text
renderReport ReportOptions {reportSnapshot} Corpus {corpusPackageCount, corpusFileCount, corpusByteCount} commit parserResults cppResults =
  T.pack $
    unlines $
      [ "# AIHC Benchmarks",
        "",
        "Generated by `nix run .#generate-benchmarks`.",
        "",
        "## Metadata",
        "",
        "| Field | Value |",
        "| --- | --- |",
        "| Stackage snapshot | `" ++ reportSnapshot ++ "` |",
        "| Commit | `" ++ commit ++ "` |",
        "| Packages | `" ++ formatInt corpusPackageCount ++ "` |",
        "| Haskell files | `" ++ formatInt corpusFileCount ++ "` |",
        "| Corpus size | `" ++ formatBytes corpusByteCount ++ "` |",
        "",
        "## Parser Performance",
        "",
        "Parser input is preprocessed with `aihc-cpp` before measurement. GHC is the baseline.",
        "",
        "| Parser | Relative Performance |",
        "| --- | ---: |"
      ]
        ++ map (renderRatioRow (baseline "GHC (`ghc-lib-parser`)" parserResults)) parserResults
        ++ [ "",
             "## CPP Performance",
             "",
             "`clang -E` is the baseline.",
             "",
             "| Preprocessor | Relative Performance |",
             "| --- | ---: |"
           ]
        ++ map (renderRatioRow (baseline "clang -E" cppResults)) cppResults

renderRatioRow :: Integer -> ToolResult -> String
renderRatioRow baselineNanos ToolResult {toolName, toolNanos} =
  "| " ++ toolName ++ " | `" ++ formatRatio baselineNanos toolNanos ++ "` |"

baseline :: String -> [ToolResult] -> Integer
baseline name results =
  case [toolNanos r | r <- results, toolName r == name] of
    n : _ -> n
    [] -> error ("missing benchmark baseline: " ++ name)

formatRatio :: Integer -> Integer -> String
formatRatio _ 0 = "0.00x"
formatRatio baselineNanos candidateNanos =
  printf "%.2fx" (fromIntegral baselineNanos / fromIntegral candidateNanos :: Double)

formatInt :: Int -> String
formatInt n
  | n < 1000 = show n
  | otherwise = formatInt (n `div` 1000) ++ "," ++ printf "%03d" (n `mod` 1000)

formatBytes :: Integer -> String
formatBytes bytes =
  let units = ["B", "KB", "MB", "GB", "TB"] :: [String]
      go value (unit : nextUnits)
        | value < 1024 || null nextUnits = printf "%.1f %s" value unit
        | otherwise = go (value / 1024) nextUnits
      go value [] = printf "%.1f B" value
   in if bytes < 1024
        then show bytes ++ " B"
        else go (fromIntegral bytes / 1024 :: Double) (drop 1 units)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    dropWhileEnd p = reverse . dropWhile p . reverse
