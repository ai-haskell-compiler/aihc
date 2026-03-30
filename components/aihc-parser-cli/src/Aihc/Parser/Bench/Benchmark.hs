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
import Aihc.Parser.Bench.Parsers (ParseResult (..), lexWithAihc, parseWithAihc, parseWithGhc, parseWithHse)
import Aihc.Parser.Bench.Tarball (TarballEntry (..), streamTarballEntries)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (evaluate)
import Control.Monad (replicateM)
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
  entries <- streamTarballEntries (benchTarball opts)
  hPutStrLn stderr $ "Loaded " ++ show (length entries) ++ " files"

  -- Force entries into memory by evaluating the list spine and each entry
  hPutStr stderr "Loading files into memory..."
  hFlush stderr
  let !forcedEntries = forceEntries entries
  hPutStrLn stderr " done"

  let parser = selectParser opts
      numWarmup = benchWarmup opts
      numMain = benchIterations opts

  -- Warmup iterations
  hPutStrLn stderr $ "Running " ++ show numWarmup ++ " warmup iteration(s)..."
  warmupResults <- replicateM numWarmup $ do
    result <- runSingleIteration parser (benchJobs opts) forcedEntries
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
    result <- runSingleIteration parser (benchJobs opts) forcedEntries
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

-- | Select the parser function based on options.
selectParser :: BenchOptions -> (TarballEntry -> ParseResult)
selectParser opts =
  case (benchParser opts, benchLexerOnly opts) of
    (ParserAihc, True) -> lexWithAihc . entryContents
    (ParserAihc, False) -> parseWithAihc . entryContents
    (ParserHse, _) -> parseWithHse . entryContents
    (ParserGhc, _) -> parseWithGhc . entryContents

-- | Run a single benchmark iteration.
runSingleIteration :: (TarballEntry -> ParseResult) -> Int -> [TarballEntry] -> IO IterationResult
runSingleIteration parser jobs entries = do
  startTime <- getMonotonicTimeNSec

  results <-
    if jobs <= 1
      then pure $ map parser entries
      else do
        let chunks = chunksOf ((length entries + jobs - 1) `div` jobs) entries
        concat <$> mapConcurrently (pure . map parser) chunks

  -- Force evaluation of all results
  _ <- evaluate (length [() | ParseSuccess <- results])

  endTime <- getMonotonicTimeNSec

  let successCount = length [() | ParseSuccess <- results]
      failCount = length results - successCount
      totalBytes = sum (map (fromIntegral . entryByteSize) entries)

  pure
    IterationResult
      { iterWallTimeNs = fromIntegral (endTime - startTime),
        iterBytesRead = totalBytes,
        iterFilesRead = length entries,
        iterParseSuccess = successCount,
        iterParseFailed = failCount
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

-- | Split a list into chunks.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest

-- | Force all entries into memory.
forceEntries :: [TarballEntry] -> [TarballEntry]
forceEntries [] = []
forceEntries (e : es) =
  let !_ = entryContents e `seq` entryByteSize e
   in e : forceEntries es
