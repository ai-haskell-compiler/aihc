module HackageProgress.Run (run) where

import Aihc.Hackage.Index
  ( HackageIndexMode (..),
    loadHackageIndex,
  )
import Control.Monad (when)
import HackageProgress.CLI (Options (..), toStackageOptions)
import StackageProgress.Run qualified as StackageProgressRun
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

run :: Options -> IO ()
run opts = do
  when (optOffline opts && optUpdateIndex opts) $ do
    hPutStrLn stderr "Cannot combine --offline with --update-index."
    exitFailure

  indexResult <- loadHackageIndex Nothing (hackageIndexMode opts)
  packages <-
    case indexResult of
      Left err -> do
        hPutStrLn stderr ("Failed to load Hackage index: " ++ err)
        exitFailure
      Right specs -> pure specs

  StackageProgressRun.runPackages (toStackageOptions opts) packages

hackageIndexMode :: Options -> HackageIndexMode
hackageIndexMode opts
  | optUpdateIndex opts = UpdateHackageIndex
  | optOffline opts = OfflineHackageIndex
  | otherwise = UseCachedHackageIndex
