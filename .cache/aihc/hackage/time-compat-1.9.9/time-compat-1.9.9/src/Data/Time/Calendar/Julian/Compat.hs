{-# LANGUAGE CPP #-}
module Data.Time.Calendar.Julian.Compat (
    Year, MonthOfYear, DayOfMonth, DayOfYear,

    -- JulianYearDay
    toJulianYearAndDay,
    fromJulianYearAndDay,
    fromJulianYearAndDayValid,
    showJulianYearAndDay,
    isJulianLeapYear,

    toJulian,fromJulian,
    pattern JulianYearMonthDay,
    fromJulianValid,showJulian,julianMonthLength,

    -- calendrical arithmetic
    -- e.g. "one month after March 31st"
    addJulianMonthsClip,addJulianMonthsRollOver,
    addJulianYearsClip,addJulianYearsRollOver,
    addJulianDurationClip,addJulianDurationRollOver,
    diffJulianDurationClip,diffJulianDurationRollOver,
) where

import Data.Time.Orphans ()

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,14,0)
import Data.Time.Calendar.Julian hiding (diffJulianDurationRollOver)
#else
import Data.Time.Calendar.Julian
#endif

import Data.Time.Calendar.Compat

#if !MIN_VERSION_time(1,11,0)
import Data.Time.Calendar.Types
#endif

#if !MIN_VERSION_time(1,9,0)

-- | Add months (clipped to last day), then add days
addJulianDurationClip :: CalendarDiffDays -> Day -> Day
addJulianDurationClip (CalendarDiffDays m d) day = addDays d $ addJulianMonthsClip m day

-- | Add months (rolling over to next month), then add days
addJulianDurationRollOver :: CalendarDiffDays -> Day -> Day
addJulianDurationRollOver (CalendarDiffDays m d) day = addDays d $ addJulianMonthsRollOver m day

-- | Calendrical difference, with as many whole months as possible
diffJulianDurationClip :: Day -> Day -> CalendarDiffDays
diffJulianDurationClip day2 day1 = let
    (y1,m1,d1) = toJulian day1
    (y2,m2,d2) = toJulian day2
    ym1 = y1 * 12 + toInteger m1
    ym2 = y2 * 12 + toInteger m2
    ymdiff = ym2 - ym1
    ymAllowed =
        if day2 >= day1 then
        if d2 >= d1 then ymdiff else ymdiff - 1
        else if d2 <= d1 then ymdiff else ymdiff + 1
    dayAllowed = addJulianDurationClip (CalendarDiffDays ymAllowed 0) day1
    in CalendarDiffDays ymAllowed $ diffDays day2 dayAllowed

#endif

#if !MIN_VERSION_time(1,14,0)

diffJulianDurationRollOver :: Day -> Day -> CalendarDiffDays
diffJulianDurationRollOver day2 day1 =
    let
        (y1, m1, _) = toJulian day1
        (y2, m2, _) = toJulian day2
        ym1 = y1 * 12 + toInteger m1
        ym2 = y2 * 12 + toInteger m2
        ymdiff = ym2 - ym1
        findpos mdiff =
            let
                dayAllowed = addJulianDurationRollOver (CalendarDiffDays mdiff 0) day1
                dd = diffDays day2 dayAllowed
            in
                if dd >= 0 then CalendarDiffDays mdiff dd else findpos (pred mdiff)
        findneg mdiff =
            let
                dayAllowed = addJulianDurationRollOver (CalendarDiffDays mdiff 0) day1
                dd = diffDays day2 dayAllowed
            in
                if dd <= 0 then CalendarDiffDays mdiff dd else findpos (succ mdiff)
    in
        if day2 >= day1
            then findpos ymdiff
            else findneg ymdiff
#endif

#if !MIN_VERSION_time(1,11,0)
-- | Bidirectional abstract constructor for the proleptic Julian calendar.
-- Invalid values will be clipped to the correct range, month first, then day.
pattern JulianYearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern JulianYearMonthDay y m d <- (toJulian -> (y,m,d)) where
    JulianYearMonthDay y m d = fromJulian y m d

{-# COMPLETE JulianYearMonthDay #-}
#endif
