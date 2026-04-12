-- | Benchmark runner for parser performance testing.
module Aihc.Parser.Bench.Benchmark
  ( -- * Types
    BenchmarkResult (..),
    IterationResult (..),
    GCStatsSnapshot (..),

    -- * Running benchmarks
    runBenchmark,
    runSingleIteration,
  )
where

import Aihc.Parser.Bench.CLI (BenchOptions (..), ParserChoice (..))
import Aihc.Parser.Bench.Parsers (ParseResult (..), lexWithAihcExts, parseWithAihcExts, parseWithGhcExts, parseWithHseExts)
import Aihc.Parser.Bench.Tarball (PackageInfo (..), PackageSpec (..), TarballEntry (..), isHaskellEntry, isIncludeEntry, streamTarball)
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats qualified as Stats
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

-- | Result of a single benchmark iteration.
data IterationResult = IterationResult
  { iterWallTimeNs :: !Integer,
    iterBytesRead :: !Integer,
    iterFilesRead :: !Int,
    iterParseSuccess :: !Int,
    iterParseFailed :: !Int
  }
  deriving (Eq, Show)

-- | GC statistics snapshot.
data GCStatsSnapshot = GCStatsSnapshot
  { gcBytesAllocated :: !Integer,
    gcBytesCopied :: !Integer,
    gcMaxLiveBytes :: !Integer,
    gcGcCpuNs :: !Integer,
    gcGcElapsedNs :: !Integer,
    gcNumGcs :: !Int
  }
  deriving (Eq, Show)

-- | Result of a full benchmark run.
data BenchmarkResult = BenchmarkResult
  { benchWarmupResults :: ![IterationResult],
    benchMainResults :: ![IterationResult],
    benchGcBefore :: !(Maybe GCStatsSnapshot),
    benchGcAfter :: !(Maybe GCStatsSnapshot)
  }
  deriving (Eq, Show)

-- | Run the full benchmark.
runBenchmark :: BenchOptions -> IO BenchmarkResult
runBenchmark opts = do
  hPutStrLn stderr $ "Loading tarball: " ++ benchTarball opts
  (rawEntries, packageInfos) <- streamTarball (benchTarball opts)

  -- Build include map from non-Haskell, non-cabal entries
  let includeEntries = filter isIncludeEntry rawEntries
      includeMap = Map.fromList [(entryFilePath e, entryContents e) | e <- includeEntries]

  -- Filter to only Haskell files and enrich with extension info
  let hsEntries = filter isHaskellEntry rawEntries
      enrichedEntries = map (enrichEntry packageInfos) hsEntries

  hPutStrLn stderr $ "Loaded " ++ show (length enrichedEntries) ++ " Haskell files"

  -- Force entries into memory by evaluating the list spine and each entry
  hPutStr stderr "Loading files into memory..."
  hFlush stderr
  let !forcedEntries = forceEntries enrichedEntries
  hPutStrLn stderr " done"

  let parser = selectParser includeMap opts
      numWarmup = benchWarmup opts
      numMain = benchIterations opts

  -- Warmup iterations
  hPutStrLn stderr $ "Running " ++ show numWarmup ++ " warmup iteration(s)..."
  warmupResults <- replicateM numWarmup $ do
    result <- runSingleIteration parser forcedEntries
    hPutStrLn stderr $
      "  Warmup: "
        ++ show (iterFilesRead result)
        ++ " files, "
        ++ show (iterWallTimeNs result `div` 1000000)
        ++ "ms"
    pure result

  -- Capture GC stats before main iterations if requested
  gcBefore <-
    if benchGcStats opts
      then do
        enabled <- Stats.getRTSStatsEnabled
        if enabled
          then Just <$> captureGCStats
          else do
            hPutStrLn stderr "Warning: GC stats requested but RTS stats not enabled. Run with +RTS -T"
            pure Nothing
      else pure Nothing

  -- Main iterations
  hPutStrLn stderr $ "Running " ++ show numMain ++ " benchmark iteration(s)..."
  mainResults <- replicateM numMain $ do
    result <- runSingleIteration parser forcedEntries
    hPutStrLn stderr $
      "  Iteration: "
        ++ show (iterFilesRead result)
        ++ " files, "
        ++ show (iterWallTimeNs result `div` 1000000)
        ++ "ms"
    pure result

  -- Capture GC stats after main iterations
  gcAfter <-
    if benchGcStats opts
      then do
        enabled <- Stats.getRTSStatsEnabled
        if enabled
          then Just <$> captureGCStats
          else pure Nothing
      else pure Nothing

  pure
    BenchmarkResult
      { benchWarmupResults = warmupResults,
        benchMainResults = mainResults,
        benchGcBefore = gcBefore,
        benchGcAfter = gcAfter
      }

-- | Enrich a tarball entry with extension info from parsed .cabal files.
enrichEntry :: Map.Map String PackageInfo -> TarballEntry -> TarballEntry
enrichEntry packageInfos entry =
  let pkgKey = pkgName (entryPackage entry) ++ "-" ++ pkgVersion (entryPackage entry)
      pkgPrefix = pkgKey ++ "/"
   in case Map.lookup pkgKey packageInfos of
        Nothing -> entry -- No package info found, use entry as-is
        Just info ->
          -- Try to find the file in the package info
          let relPath = dropPrefix pkgPrefix (entryFilePath entry)
           in case Map.lookup relPath (packageInfoFiles info) of
                Nothing ->
                  -- Try without leading directory
                  let pathsToTry =
                        [ relPath,
                          -- Try stripping subdirectory
                          dropFirstDir relPath,
                          -- Try with different source dirs
                          "src/" ++ relPath,
                          "lib/" ++ relPath
                        ]
                      lookupResult = firstJust (map (`Map.lookup` (packageInfoFiles info)) pathsToTry)
                   in case lookupResult of
                        Just (exts, cppOpts, lang, deps) ->
                          entry {entryExtensions = exts, entryCppOptions = cppOpts, entryLanguage = lang, entryDependencies = deps}
                        Nothing -> entry
                Just (exts, cppOpts, lang, deps) ->
                  entry {entryExtensions = exts, entryCppOptions = cppOpts, entryLanguage = lang, entryDependencies = deps}
  where
    dropPrefix prefix s =
      if prefix `isPrefixOf` s
        then drop (length prefix) s
        else s

    dropFirstDir path =
      case break (== '/') path of
        (_, '/' : rest) -> rest
        _ -> path

    firstJust = listToMaybe . catMaybes

-- | Select the parser function based on options.
-- Now uses the entry's extensions, language, and include map for CPP resolution.
selectParser :: Map.Map FilePath Text -> BenchOptions -> (TarballEntry -> ParseResult)
selectParser includeMap opts =
  case (benchParser opts, benchLexerOnly opts) of
    (ParserAihc, True) -> \e -> lexWithAihcExts includeMap (entryFilePath e) (entryExtensions e) (entryCppOptions e) (entryLanguage e) (entryDependencies e) (entryContents e)
    (ParserAihc, False) -> \e -> parseWithAihcExts includeMap (entryFilePath e) (entryExtensions e) (entryCppOptions e) (entryLanguage e) (entryDependencies e) (entryContents e)
    (ParserHse, _) -> \e -> parseWithHseExts includeMap (entryFilePath e) (entryExtensions e) (entryCppOptions e) (entryLanguage e) (entryDependencies e) (entryContents e)
    (ParserGhc, _) -> \e -> parseWithGhcExts includeMap (entryFilePath e) (entryExtensions e) (entryCppOptions e) (entryLanguage e) (entryDependencies e) (entryContents e)

-- | Run a single benchmark iteration.
runSingleIteration :: (TarballEntry -> ParseResult) -> [TarballEntry] -> IO IterationResult
runSingleIteration parser entries = do
  startTime <- getMonotonicTimeNSec

  let results = map parser entries

  -- Force full evaluation of all results using rnf
  _ <- evaluate (rnf results)

  endTime <- getMonotonicTimeNSec

  let successCount = length [() | ParseSuccess <- results]
      totalBytes = sum (map (fromIntegral . entryByteSize) entries)

  pure
    IterationResult
      { iterWallTimeNs = fromIntegral (endTime - startTime),
        iterBytesRead = totalBytes,
        iterFilesRead = length entries,
        iterParseSuccess = successCount,
        iterParseFailed = length results - successCount
      }

-- | Capture current GC statistics.
captureGCStats :: IO GCStatsSnapshot
captureGCStats = do
  stats <- Stats.getRTSStats
  pure
    GCStatsSnapshot
      { gcBytesAllocated = fromIntegral (Stats.allocated_bytes stats),
        gcBytesCopied = fromIntegral (Stats.copied_bytes stats),
        gcMaxLiveBytes = fromIntegral (Stats.max_live_bytes stats),
        gcGcCpuNs = fromIntegral (Stats.gc_cpu_ns stats),
        gcGcElapsedNs = fromIntegral (Stats.gc_elapsed_ns stats),
        gcNumGcs = fromIntegral (Stats.gcs stats)
      }

-- | Force all entries into memory.
-- Uses strict fold to ensure all entries are fully evaluated.
forceEntries :: [TarballEntry] -> [TarballEntry]
forceEntries = foldr forceAndCons []
  where
    forceAndCons e !acc =
      let !_ = entryContents e `seq` entryByteSize e `seq` entryExtensions e `seq` entryCppOptions e `seq` entryLanguage e `seq` entryDependencies e
       in e : acc
