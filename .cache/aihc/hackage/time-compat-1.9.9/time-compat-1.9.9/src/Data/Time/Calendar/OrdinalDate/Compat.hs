{-# LANGUAGE CPP #-}
module Data.Time.Calendar.OrdinalDate.Compat (
    Day, Year, DayOfYear, WeekOfYear,
    toOrdinalDate,
    fromOrdinalDate,
    pattern YearDay,
    fromOrdinalDateValid,
    showOrdinalDate,
    isLeapYear,
    mondayStartWeek,
    sundayStartWeek,
    fromMondayStartWeek,
    fromMondayStartWeekValid,
    fromSundayStartWeek,
    fromSundayStartWeekValid,
) where

import Data.Time.Orphans ()

import Data.Time.Calendar.OrdinalDate hiding (fromSundayStartWeekValid)
import Data.Time.Calendar.OrdinalDate (fromSundayStartWeekValid)

#if !MIN_VERSION_time(1,11,0)
import Data.Time.Calendar
import Data.Time.Calendar.Types
#endif

#if !MIN_VERSION_time(1,11,0)

-- | Bidirectional abstract constructor for ISO 8601 Ordinal Date format.
-- Invalid day numbers will be clipped to the correct range (1 to 365 or 366).
pattern YearDay :: Year -> DayOfYear -> Day
pattern YearDay y d <- (toOrdinalDate -> (y,d)) where
    YearDay y d = fromOrdinalDate y d

{-# COMPLETE YearDay #-}

#endif
