{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Time.Calendar.DayPeriod (
    DayPeriod (..),
    periodAllDays,
    periodLength,
    periodFromDay,
    periodToDay,
    periodToDayClip,
    periodToDayValid,
) where

#if MIN_VERSION_time(1,15,0)
import Data.Time.Calendar

#elif MIN_VERSION_time(1,12,1)
import Data.Time.Calendar
import Data.Time.Calendar.Private

periodToDayClip :: DayPeriod p => p -> Int -> Day
periodToDayClip p i = periodToDay p $ clip 1 (periodLength p) i

#else
import Data.Time.Calendar
import Data.Time.Calendar.Private

#if MIN_VERSION_time(1,11,0)
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
#endif

class Ord p => DayPeriod p where
    -- | Returns the first 'Day' in a period of days.
    periodFirstDay :: p -> Day

    -- | Returns the last 'Day' in a period of days.
    periodLastDay :: p -> Day

    -- | Get the period this day is in.
    dayPeriod :: Day -> p

-- | A list of all the days in this period.
--
-- @since 1.12.1
periodAllDays :: DayPeriod p => p -> [Day]
periodAllDays p = [periodFirstDay p .. periodLastDay p]

-- | The number of days in this period.
--
-- @since 1.12.1
periodLength :: DayPeriod p => p -> Int
periodLength p = succ $ fromInteger $ diffDays (periodLastDay p) (periodFirstDay p)

-- | Get the period this day is in, with the 1-based day number within the period.
--
-- @periodFromDay (periodFirstDay p) = (p,1)@
--
-- @since 1.12.1
periodFromDay :: DayPeriod p => Day -> (p, Int)
periodFromDay d =
    let
        p = dayPeriod d
        dt = succ $ fromInteger $ diffDays d $ periodFirstDay p
    in
        (p, dt)

-- | Inverse of 'periodFromDay'.
--
-- @since 1.12.1
periodToDay :: DayPeriod p => p -> Int -> Day
periodToDay p i = addDays (toInteger $ pred i) $ periodFirstDay p

periodToDayClip :: DayPeriod p => p -> Int -> Day
periodToDayClip p i = periodToDay p $ clip 1 (periodLength p) i

-- | Validating inverse of 'periodFromDay'.
--
-- @since 1.12.1
periodToDayValid :: DayPeriod p => p -> Int -> Maybe Day
periodToDayValid p i =
    let
        d = periodToDay p i
    in
        if fst (periodFromDay d) == p then Just d else Nothing

instance DayPeriod Day where
    periodFirstDay = id
    periodLastDay = id
    dayPeriod = id

-- Year instance
instance DayPeriod Integer where
    periodFirstDay y = fromGregorian y 1 1
    periodLastDay y = fromGregorian y 12 31
    dayPeriod (toGregorian -> (y,_,_)) = y

#if MIN_VERSION_time(1,11,0)
instance DayPeriod Month where
    periodFirstDay (YearMonth y m) = YearMonthDay y m 1
    periodLastDay (YearMonth y m) = YearMonthDay y m 31 -- clips to correct day
    dayPeriod (YearMonthDay y my _) = YearMonth y my

instance DayPeriod Quarter where
    periodFirstDay (YearQuarter y q) =
        case q of
            Q1 -> periodFirstDay $ YearMonth y 1
            Q2 -> periodFirstDay $ YearMonth y 4
            Q3 -> periodFirstDay $ YearMonth y 7
            Q4 -> periodFirstDay $ YearMonth y 10
    periodLastDay (YearQuarter y q) =
        case q of
            Q1 -> periodLastDay $ YearMonth y 3
            Q2 -> periodLastDay $ YearMonth y 6
            Q3 -> periodLastDay $ YearMonth y 9
            Q4 -> periodLastDay $ YearMonth y 12
    dayPeriod (MonthDay m _) = monthQuarter m
#endif

#endif
