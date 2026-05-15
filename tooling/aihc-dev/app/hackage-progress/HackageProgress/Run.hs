module HackageProgress.Run (run) where

import Aihc.Hackage.Index
  ( HackageIndexMode (..),
    loadHackageIndexUpdatedSince,
  )
import Control.Monad (when)
import Data.Int (Int64)
import Data.Time.Calendar (Day, addGregorianYearsClip)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import HackageProgress.CLI (Options (..), toStackageOptions)
import StackageProgress.Run qualified as StackageProgressRun
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

run :: Options -> IO ()
run opts = do
  when (optOffline opts && optUpdateIndex opts) $ do
    hPutStrLn stderr "Cannot combine --offline with --update-index."
    exitFailure

  cutoff <- cutoffUnixTime opts
  indexResult <- loadHackageIndexUpdatedSince Nothing (hackageIndexMode opts) cutoff
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

cutoffUnixTime :: Options -> IO Int64
cutoffUnixTime opts =
  dayToUnixTime <$> maybe fiveYearsAgo pure (optUpdatedSince opts)

fiveYearsAgo :: IO Day
fiveYearsAgo =
  addGregorianYearsClip (-5) . utctDay <$> getCurrentTime

dayToUnixTime :: Day -> Int64
dayToUnixTime day =
  floor (utcTimeToPOSIXSeconds (UTCTime day 0))
