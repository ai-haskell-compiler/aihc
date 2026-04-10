-- | CLI entry point for aihc-parser-bench.
module Aihc.Parser.CLI.Bench
  ( main,
  )
where

import Aihc.Parser.Bench.Benchmark (runBenchmark)
import Aihc.Parser.Bench.CLI
  ( BenchOptions (..),
    Command (..),
    GenerateOptions (..),
    Options (..),
    OutputFormat (..),
    parseOptionsIO,
  )
import Aihc.Parser.Bench.Metrics (computeMetrics, formatBytes, formatCsv, formatCsvHeader, formatHuman, formatJson)
import Aihc.Parser.Bench.Tarball (FilterReason (..), GenerateResult (..), PackageSpec (..), formatPackage, generateTarball)
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- | Main entry point.
main :: IO ()
main = do
  opts <- parseOptionsIO
  case optCommand opts of
    CmdGenerate genOpts -> runGenerate genOpts
    CmdBench benchOpts -> runBench benchOpts

-- | Run the generate command.
runGenerate :: GenerateOptions -> IO ()
runGenerate opts = do
  result <- generateTarball opts
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      exitFailure
    Right summary -> do
      printGenerateSummary opts summary
      if resultIncludedPackages summary > 0
        then exitSuccess
        else exitFailure

-- | Print summary of tarball generation.
printGenerateSummary :: GenerateOptions -> GenerateResult -> IO ()
printGenerateSummary opts summary = do
  putStrLn ""
  putStrLn "Generation Summary"
  putStrLn "=================="
  putStrLn $ "Snapshot:           " ++ genSnapshot opts
  putStrLn $ "Total packages:     " ++ show (resultTotalPackages summary)
  putStrLn $ "Included packages:  " ++ show (resultIncludedPackages summary)
  putStrLn $ "Total files:        " ++ show (resultTotalFiles summary)
  putStrLn $ "Total bytes:        " ++ formatBytes (resultTotalBytes summary)

  let filtered = resultFilteredOut summary
  if null filtered
    then putStrLn "Filtered out:       0"
    else do
      putStrLn $ "Filtered out:       " ++ show (length filtered)
      when (genVerbose opts) $ do
        putStrLn "\nFiltered packages:"
        mapM_ (printFilteredPackage opts) (take 20 filtered)
        when (length filtered > 20) $
          putStrLn $
            "  ... and " ++ show (length filtered - 20) ++ " more"

-- | Print why a package was filtered.
printFilteredPackage :: GenerateOptions -> (PackageSpec, FilterReason) -> IO ()
printFilteredPackage _ (pkg, reason) =
  putStrLn $ "  " ++ formatPackage pkg ++ ": " ++ reasonToString reason

reasonToString :: FilterReason -> String
reasonToString (FilterAihcFailed path _) = "aihc-parser failed: " ++ path
reasonToString (FilterHseFailed path _) = "haskell-src-exts failed: " ++ path
reasonToString (FilterGhcFailed path _) = "ghc-lib-parser failed: " ++ path
reasonToString FilterNoHaskellFiles = "no Haskell files"
reasonToString (FilterDownloadFailed err) = "download failed: " ++ err
reasonToString (FilterCabalParseFailed err) = "cabal file parse failed: " ++ err

-- | Run the bench command.
runBench :: BenchOptions -> IO ()
runBench opts = do
  result <- runBenchmark opts
  let metrics = computeMetrics opts result

  case benchOutput opts of
    FormatHuman -> putStrLn (formatHuman opts metrics)
    FormatJson -> LBS8.putStrLn (formatJson opts metrics)
    FormatCsv -> do
      putStrLn formatCsvHeader
      putStrLn (formatCsv opts metrics)

  exitSuccess
