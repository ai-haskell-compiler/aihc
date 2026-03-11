module System.Path.ModificationTime where

import qualified System.Time as SysTime
import qualified Data.Time as Time
import Data.Time (UTCTime(UTCTime))

-- directory < 1.2 used old-time;
-- to present a consistent API we convert it to time:Data.Time.UTCTime.
convertTime :: SysTime.ClockTime -> UTCTime
convertTime clock = UTCTime day (fromIntegral sec)
  where
    calendar = SysTime.toUTCTime clock
    day = Time.addDays (toInteger (SysTime.ctYDay calendar)) yearStart
    yearStart = Time.fromGregorian (toInteger (SysTime.ctYear calendar)) 1 1
    hour = SysTime.ctHour calendar
    minu = hour * 60 + SysTime.ctMin calendar
    sec = minu * 60 + SysTime.ctSec calendar
