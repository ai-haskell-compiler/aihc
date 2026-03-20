module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import ParserQuickCheck.CLI (optMaxSuccess, optProperty, optSeed, parseOptionsIO)
import ParserQuickCheck.Registry (registeredParserProperties)
import ParserQuickCheck.Runner (BatchConfig (..), resolveBatchSeed, runBatch, selectProperties)
import ParserQuickCheck.Types (results, status, statusRequiresAttention)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options <- parseOptionsIO
  resolvedBatchSeed <- resolveBatchSeed (optSeed options)
  now <- getCurrentTime
  commitShaValue <- fmap (T.pack . fromMaybe "unknown") (lookupEnv "AIHC_COMMIT_SHA")
  case selectProperties (T.pack <$> optProperty options) registeredParserProperties of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right selectedProperties -> do
      batchResult <-
        runBatch
          now
          BatchConfig
            { batchCommitSha = commitShaValue,
              batchMaxSuccess = optMaxSuccess options,
              batchSeedValue = resolvedBatchSeed
            }
          selectedProperties
      BL8.putStrLn (Aeson.encode batchResult)
      if any (statusRequiresAttention . status) (results batchResult)
        then exitFailure
        else exitSuccess
