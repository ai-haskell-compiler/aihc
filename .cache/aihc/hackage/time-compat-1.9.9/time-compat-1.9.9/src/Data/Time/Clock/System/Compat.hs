module Data.Time.Clock.System.Compat (
    systemEpochDay,
    SystemTime(..),
    truncateSystemTimeLeapSecond,
    getSystemTime,
    systemToUTCTime,
    utcToSystemTime,
    systemToTAITime,
    ) where

import Data.Time.Orphans ()
import Data.Time.Clock.System
