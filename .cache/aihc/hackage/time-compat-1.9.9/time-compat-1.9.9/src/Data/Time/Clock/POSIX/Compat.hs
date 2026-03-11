module Data.Time.Clock.POSIX.Compat (
    posixDayLength,POSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,getPOSIXTime,getCurrentTime,
    systemToPOSIXTime,
    ) where

import Data.Time.Orphans ()

import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Clock.System.Compat
