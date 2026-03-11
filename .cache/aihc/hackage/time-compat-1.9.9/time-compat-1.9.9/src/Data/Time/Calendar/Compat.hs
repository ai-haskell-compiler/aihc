{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
module Data.Time.Calendar.Compat (
    -- * Days
    Day(..),addDays,diffDays,

    -- * DayPeriod
    DayPeriod (..),
    periodAllDays,
    periodLength,
    periodFromDay,
    periodToDay,
    periodToDayValid,

    -- * CalendarDiffTime
    CalendarDiffDays (..),
    calendarDay,calendarWeek,calendarMonth,calendarYear,scaleCalendarDiffDays,


    -- * Year, month and day
    Year,
    pattern CommonEra,
    pattern BeforeCommonEra,
    MonthOfYear,
    pattern January,
    pattern February,
    pattern March,
    pattern April,
    pattern May,
    pattern June,
    pattern July,
    pattern August,
    pattern September,
    pattern October,
    pattern November,
    pattern December,
    DayOfMonth,

    -- * Gregorian calendar
    toGregorian,fromGregorian,fromGregorianValid,showGregorian,gregorianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addGregorianMonthsClip,addGregorianMonthsRollOver,
    addGregorianYearsClip,addGregorianYearsRollOver,
    addGregorianDurationClip,addGregorianDurationRollOver,
    diffGregorianDurationClip,diffGregorianDurationRollOver,

    -- re-exported from OrdinalDate
    isLeapYear ,

      -- * Week
    DayOfWeek(..), dayOfWeek,
    dayOfWeekDiff, firstDayOfWeekOnAfter,
    weekAllDays,
    weekFirstDay,
    weekLastDay,

    -- * Type aliases
    pattern YearMonthDay,
) where

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,14,0)
import Data.Time.Calendar hiding (diffGregorianDurationRollOver)
#else
import Data.Time.Calendar
#endif

import Data.Time.Format
import Data.Time.Orphans ()

#if !MIN_VERSION_time(1,9,0)
import Data.Time.Calendar.WeekDate.Compat
#endif

#if !MIN_VERSION_time(1,12,0)
import Data.Time.Calendar.MonthDay.Compat
#endif

#if !MIN_VERSION_time(1,12,1)
import Data.Time.Calendar.Types
import Data.Time.Calendar.Month.Compat
import Data.Time.Calendar.Quarter.Compat
#endif

import Data.Time.Calendar.Types
import Data.Time.Calendar.DayPeriod

import Control.DeepSeq (NFData (..))
import Data.Data       (Data, Typeable)
import GHC.Generics   (Generic)
import Data.Monoid     (Monoid (..))
import Data.Semigroup  (Semigroup (..))
import qualified Language.Haskell.TH.Syntax as TH


-------------------------------------------------------------------------------
-- CalendarDiffTime
-------------------------------------------------------------------------------

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,9,2)
deriving instance Typeable CalendarDiffDays
deriving instance Data CalendarDiffDays
#endif

#if !MIN_VERSION_time(1,9,0)

data CalendarDiffDays = CalendarDiffDays
    { cdMonths :: Integer
    , cdDays :: Integer
    } deriving (Eq, Data, Typeable, Generic, TH.Lift)

-- | Additive
instance Semigroup CalendarDiffDays where
    CalendarDiffDays m1 d1 <> CalendarDiffDays m2 d2 = CalendarDiffDays (m1 + m2) (d1 + d2)

-- | Additive
instance Monoid CalendarDiffDays where
    mempty  = CalendarDiffDays 0 0
    mappend = (<>)

instance Show CalendarDiffDays where
    show (CalendarDiffDays m d) = "P" ++ show m ++ "M" ++ show d ++ "D"

instance NFData CalendarDiffDays where
    rnf (CalendarDiffDays x y) = rnf x `seq` rnf y

calendarDay :: CalendarDiffDays
calendarDay = CalendarDiffDays 0 1

calendarWeek :: CalendarDiffDays
calendarWeek = CalendarDiffDays 0 7

calendarMonth :: CalendarDiffDays
calendarMonth = CalendarDiffDays 1 0

calendarYear :: CalendarDiffDays
calendarYear = CalendarDiffDays 12 0

-- | Scale by a factor. Note that @scaleCalendarDiffDays (-1)@ will not perfectly invert a duration, due to variable month lengths.
scaleCalendarDiffDays :: Integer -> CalendarDiffDays -> CalendarDiffDays
scaleCalendarDiffDays k (CalendarDiffDays m d) = CalendarDiffDays (k * m) (k * d)

#endif

-- TODO:
-- instance Read CalendarDiffDays where
--    readsPrec = error "TODO"

-------------------------------------------------------------------------------
-- Gregorian
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,9,0)

-- | Add months (clipped to last day), then add days
addGregorianDurationClip :: CalendarDiffDays -> Day -> Day
addGregorianDurationClip (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addGregorianDurationRollOver :: CalendarDiffDays -> Day -> Day
addGregorianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addGregorianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffGregorianDurationClip :: Day -> Day -> CalendarDiffDays
diffGregorianDurationClip day2 day1 = let
    (y1,m1,d1) = toGregorian day1
    (y2,m2,d2) = toGregorian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addGregorianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed
#endif

#if !MIN_VERSION_time(1,14,0)
-- | Calendrical difference, with as many whole months as possible.
diffGregorianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffGregorianDurationRollOver day2 day1 =
    let
        (y1, m1, _) = toGregorian day1
        (y2, m2, _) = toGregorian day2
        ym1 = y1 * 12 + toInteger m1
        ym2 = y2 * 12 + toInteger m2
        ymdiff = ym2 - ym1
        findpos mdiff =
            let
                dayAllowed = addGregorianDurationRollOver (CalendarDiffDays mdiff 0) day1
                dd = diffDays day2 dayAllowed
            in
                if dd >= 0 then CalendarDiffDays mdiff dd else findpos (pred mdiff)
        findneg mdiff =
            let
                dayAllowed = addGregorianDurationRollOver (CalendarDiffDays mdiff 0) day1
                dd = diffDays day2 dayAllowed
            in
                if dd <= 0 then CalendarDiffDays mdiff dd else findpos (succ mdiff)
    in
        if day2 >= day1
            then findpos ymdiff
            else findneg ymdiff
#endif

#if !MIN_VERSION_time(1,11,0)
-- | Bidirectional abstract constructor for the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range, month first, then day.
pattern YearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern YearMonthDay y m d <- (toGregorian -> (y,m,d)) where
    YearMonthDay y m d = fromGregorian y m d

{-# COMPLETE YearMonthDay #-}
#endif

-------------------------------------------------------------------------------
-- DayOfWeek
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,11,0)
-- | @dayOfWeekDiff a b = a - b@ in range 0 to 6.
-- The number of days from b to the next a.
dayOfWeekDiff :: DayOfWeek -> DayOfWeek -> Int
dayOfWeekDiff a b = mod (fromEnum a - fromEnum b) 7

-- | The first day-of-week on or after some day
firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = addDays (toInteger $ dayOfWeekDiff dw $ dayOfWeek d) d
#endif

#if !MIN_VERSION_time(1,12,2)
-- | Returns a week containing the given 'Day' where the first day is the
-- 'DayOfWeek' specified.
--
-- Examples:
--
-- >>> weekAllDays Sunday (YearMonthDay 2022 02 21)
-- [YearMonthDay 2022 2 20 .. YearMonthDay 2022 2 26]
--
-- >>> weekAllDays Monday (YearMonthDay 2022 02 21)
-- [YearMonthDay 2022 2 21 .. YearMonthDay 2022 2 27]
--
-- >>> weekAllDays Tuesday (YearMonthDay 2022 02 21)
-- [YearMonthDay 2022 2 15 .. YearMonthDay 2022 2 21]
--
-- @since 1.12.2
weekAllDays :: DayOfWeek -> Day -> [Day]
weekAllDays firstDay day = [weekFirstDay firstDay day .. weekLastDay firstDay day]

-- | Returns the first day of a week containing the given 'Day'.
--
-- Examples:
--
-- >>> weekFirstDay Sunday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 20
--
-- >>> weekFirstDay Monday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 21
--
-- >>> weekFirstDay Tuesday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 15
--
-- @since 1.12.2
weekFirstDay :: DayOfWeek -> Day -> Day
weekFirstDay firstDay day = addDays (negate 7) $ firstDayOfWeekOnAfter firstDay $ succ day

-- | Returns the last day of a week containing the given 'Day'.
--
-- Examples:
--
-- >>> weekLastDay Sunday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 26
--
-- >>> weekLastDay Monday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 27
--
-- >>> weekLastDay Tuesday (YearMonthDay 2022 02 21)
-- YearMonthDay 2022 2 21
--
-- @since 1.12.2
weekLastDay :: DayOfWeek -> Day -> Day
weekLastDay firstDay day = pred $ firstDayOfWeekOnAfter firstDay $ succ day
#endif
