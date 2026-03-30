{-# LANGUAGE OverloadedStrings #-}

-- | Metrics computation and formatting for benchmark results.
module Aihc.Parser.Bench.Metrics
  ( -- * Types
    Metrics (..),

    -- * Computation
    computeMetrics,
    computeGCMetrics,
    GCMetrics (..),

    -- * Formatting
    formatHuman,
    formatJson,
    formatCsv,
    formatCsvHeader,
  )
where

import Aihc.Parser.Bench.Benchmark (BenchmarkResult (..), GCStatsSnapshot (..), IterationResult (..))
import Aihc.Parser.Bench.CLI (BenchOptions (..), ParserChoice (..))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Text.Printf (printf)

-- | Computed metrics from benchmark results.
data Metrics = Metrics
  { -- | Total bytes processed
    metricsTotalBytes :: !Integer,
    -- | Total files processed
    metricsTotalFiles :: !Int,
    -- | Total wall-clock time in milliseconds
    metricsWallTimeMs :: !Double,
    -- | Throughput in bytes per second
    metricsBytesPerSec :: !Double,
    -- | Throughput in files per second
    metricsFilesPerSec :: !Double,
    -- | Number of successfully parsed files
    metricsParseSuccess :: !Int,
    -- | Number of failed parses
    metricsParseFailed :: !Int,
    -- | Parse success rate as a percentage
    metricsSuccessRate :: !Double,
    -- | GC-related metrics if available
    metricsGC :: !(Maybe GCMetrics)
  }
  deriving (Eq, Show)

-- | GC-specific metrics.
data GCMetrics = GCMetrics
  { -- | Total bytes allocated during benchmark
    gcTotalAllocated :: !Integer,
    -- | Total bytes copied during GC
    gcTotalCopied :: !Integer,
    -- | Maximum live bytes observed
    gcMaxLive :: !Integer,
    -- | Total GC CPU time in milliseconds
    gcCpuTimeMs :: !Double,
    -- | Total GC elapsed time in milliseconds
    gcElapsedTimeMs :: !Double,
    -- | Number of GCs during benchmark
    gcCount :: !Int
  }
  deriving (Eq, Show)

-- | Compute metrics from benchmark results.
computeMetrics :: BenchOptions -> BenchmarkResult -> Metrics
computeMetrics _opts result =
  let iterations = benchMainResults result
      -- Use median timing for stability
      times = sort (map iterWallTimeNs iterations)
      medianTimeNs = case times of
        [] -> 0
        _ -> times !! (length times `div` 2)
      wallTimeMs = fromIntegral medianTimeNs / 1000000.0

      -- Use first iteration's file/byte counts (they should all be the same)
      (totalBytes, totalFiles, successCount, failCount) = case iterations of
        [] -> (0, 0, 0, 0)
        i : _ -> (iterBytesRead i, iterFilesRead i, iterParseSuccess i, iterParseFailed i)

      bytesPerSec =
        if wallTimeMs > 0
          then fromIntegral totalBytes * 1000.0 / wallTimeMs
          else 0
      filesPerSec =
        if wallTimeMs > 0
          then fromIntegral totalFiles * 1000.0 / wallTimeMs
          else 0
      successRate =
        if totalFiles > 0
          then fromIntegral successCount * 100.0 / fromIntegral totalFiles
          else 100.0

      gcMetrics = computeGCMetrics (benchGcBefore result) (benchGcAfter result)
   in Metrics
        { metricsTotalBytes = totalBytes,
          metricsTotalFiles = totalFiles,
          metricsWallTimeMs = wallTimeMs,
          metricsBytesPerSec = bytesPerSec,
          metricsFilesPerSec = filesPerSec,
          metricsParseSuccess = successCount,
          metricsParseFailed = failCount,
          metricsSuccessRate = successRate,
          metricsGC = gcMetrics
        }

-- | Compute GC metrics from before/after snapshots.
computeGCMetrics :: Maybe GCStatsSnapshot -> Maybe GCStatsSnapshot -> Maybe GCMetrics
computeGCMetrics (Just before) (Just after) =
  Just
    GCMetrics
      { gcTotalAllocated = gcBytesAllocated after - gcBytesAllocated before,
        gcTotalCopied = gcBytesCopied after - gcBytesCopied before,
        gcMaxLive = max (gcMaxLiveBytes before) (gcMaxLiveBytes after),
        gcCpuTimeMs = fromIntegral (gcGcCpuNs after - gcGcCpuNs before) / 1000000.0,
        gcElapsedTimeMs = fromIntegral (gcGcElapsedNs after - gcGcElapsedNs before) / 1000000.0,
        gcCount = gcNumGcs after - gcNumGcs before
      }
computeGCMetrics _ _ = Nothing

-- | Format metrics for human-readable output.
formatHuman :: BenchOptions -> Metrics -> String
formatHuman opts m =
  unlines $
    [ "",
      "Benchmark Results (" ++ parserName (benchParser opts) ++ modeStr ++ ")",
      replicate 50 '=',
      "",
      "Input:",
      "  Total files:      " ++ formatInt (metricsTotalFiles m),
      "  Total bytes:      " ++ formatBytes (metricsTotalBytes m),
      "",
      "Timing:",
      "  Wall time:        " ++ printf "%.2f ms" (metricsWallTimeMs m),
      "  Throughput:       " ++ formatThroughput (metricsBytesPerSec m),
      "  Parse rate:       " ++ printf "%.1f files/sec" (metricsFilesPerSec m),
      "",
      "Results:",
      "  Success:          " ++ formatInt (metricsParseSuccess m),
      "  Failed:           " ++ formatInt (metricsParseFailed m),
      "  Success rate:     " ++ printf "%.1f%%" (metricsSuccessRate m)
    ]
      ++ maybe [] formatGCHuman (metricsGC m)
  where
    modeStr =
      if benchLexerOnly opts && benchParser opts == ParserAihc
        then ", lexer-only"
        else ""

formatGCHuman :: GCMetrics -> [String]
formatGCHuman gc =
  [ "",
    "GC Statistics:",
    "  Allocated:        " ++ formatBytes (gcTotalAllocated gc),
    "  Copied:           " ++ formatBytes (gcTotalCopied gc),
    "  Max live:         " ++ formatBytes (gcMaxLive gc),
    "  GC CPU time:      " ++ printf "%.2f ms" (gcCpuTimeMs gc),
    "  GC elapsed time:  " ++ printf "%.2f ms" (gcElapsedTimeMs gc),
    "  GC count:         " ++ show (gcCount gc)
  ]

-- | Format metrics as JSON.
formatJson :: BenchOptions -> Metrics -> LBS.ByteString
formatJson opts m =
  Aeson.encode $
    Aeson.object $
      [ "parser" .= parserName (benchParser opts),
        "lexer_only" .= (benchLexerOnly opts && benchParser opts == ParserAihc),
        "total_files" .= metricsTotalFiles m,
        "total_bytes" .= metricsTotalBytes m,
        "wall_time_ms" .= metricsWallTimeMs m,
        "bytes_per_sec" .= metricsBytesPerSec m,
        "files_per_sec" .= metricsFilesPerSec m,
        "parse_success" .= metricsParseSuccess m,
        "parse_failed" .= metricsParseFailed m,
        "success_rate" .= metricsSuccessRate m
      ]
        ++ maybe [] gcToJson (metricsGC m)
  where
    gcToJson gc =
      [ "gc_allocated" .= gcTotalAllocated gc,
        "gc_copied" .= gcTotalCopied gc,
        "gc_max_live" .= gcMaxLive gc,
        "gc_cpu_time_ms" .= gcCpuTimeMs gc,
        "gc_elapsed_time_ms" .= gcElapsedTimeMs gc,
        "gc_count" .= gcCount gc
      ]

-- | Format CSV header.
formatCsvHeader :: String
formatCsvHeader =
  "parser,lexer_only,total_files,total_bytes,wall_time_ms,bytes_per_sec,files_per_sec,parse_success,parse_failed,success_rate"

-- | Format metrics as a CSV row.
formatCsv :: BenchOptions -> Metrics -> String
formatCsv opts m =
  intercalateComma
    [ parserName (benchParser opts),
      if benchLexerOnly opts && benchParser opts == ParserAihc then "true" else "false",
      show (metricsTotalFiles m),
      show (metricsTotalBytes m),
      printf "%.2f" (metricsWallTimeMs m),
      printf "%.2f" (metricsBytesPerSec m),
      printf "%.2f" (metricsFilesPerSec m),
      show (metricsParseSuccess m),
      show (metricsParseFailed m),
      printf "%.2f" (metricsSuccessRate m)
    ]

-- | Get parser name as a string.
parserName :: ParserChoice -> String
parserName ParserAihc = "aihc"
parserName ParserHse = "hse"
parserName ParserGhc = "ghc"

-- | Format an integer with thousand separators.
formatInt :: Int -> String
formatInt n
  | n < 1000 = show n
  | otherwise = formatInt (n `div` 1000) ++ "," ++ printf "%03d" (n `mod` 1000)

-- | Format bytes with appropriate unit.
formatBytes :: Integer -> String
formatBytes b
  | b < 1024 = show b ++ " B"
  | b < 1024 * 1024 = printf "%.1f KB" (fromIntegral b / 1024.0 :: Double)
  | b < 1024 * 1024 * 1024 = printf "%.1f MB" (fromIntegral b / (1024.0 * 1024.0) :: Double)
  | otherwise = printf "%.2f GB" (fromIntegral b / (1024.0 * 1024.0 * 1024.0) :: Double)

-- | Format throughput with appropriate unit.
formatThroughput :: Double -> String
formatThroughput bps
  | bps < 1024 = printf "%.1f B/s" bps
  | bps < 1024 * 1024 = printf "%.1f KB/s" (bps / 1024.0)
  | bps < 1024 * 1024 * 1024 = printf "%.1f MB/s" (bps / (1024.0 * 1024.0))
  | otherwise = printf "%.2f GB/s" (bps / (1024.0 * 1024.0 * 1024.0))

-- | Join strings with commas.
intercalateComma :: [String] -> String
intercalateComma [] = ""
intercalateComma [x] = x
intercalateComma (x : xs) = x ++ "," ++ intercalateComma xs
