{-# LANGUAGE CPP #-}
{-
Compatibility wrapper to help manage the transition from
old-time to time packages. Only used at all on win32.
-}
module System.PosixCompat.Internal.Time (
      ClockTime
    , getClockTime
    , clockTimeToEpochTime
    ) where

import System.Posix.Types (EpochTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

type ClockTime = POSIXTime

getClockTime :: IO ClockTime
getClockTime = getPOSIXTime

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime = fromInteger . floor
