-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZTime
  (
  -- * TZTime
    Internal.TZTime
  , Internal.tzTimeLocalTime
  , Internal.tzTimeTZInfo
  , Internal.tzTimeOffset
  -- * Constructors
  , getCurrentTZTime
  , Internal.fromUTC
  , Internal.fromPOSIXTime
  , Internal.fromZonedTime
  -- ** From @LocalTime@
  , Internal.fromLocalTime
  , Internal.fromLocalTimeStrict
  , Internal.fromLocalTimeThrow
  , Internal.unsafeFromLocalTime
  , Internal.TZError(..)
  -- * Conversions
  , Internal.toUTC
  , Internal.toPOSIXTime
  , Internal.toZonedTime
  , Internal.inTZ
  -- * Modifying a TZTime
  , atEarliestOffset
  , atLatestOffset
  , atStartOfDay
  -- * Universal time-line
  -- ** Adding seconds\/minutes\/hours
  , addTime
  , hours
  , minutes
  , seconds
  -- * Local time-line
  , modifyLocal
  , modifyLocalStrict
  , modifyLocalThrow
  -- ** Adding days\/weeks\/months\/years.
  -- | Use these with one of the @modifyLocal*@ functions.
  , addCalendarClip
  , addCalendarRollOver
  , calendarDays
  , calendarWeeks
  , calendarMonths
  , calendarYears
  -- ** Setting date\/time components.
  -- | Use these with one of the @modifyLocal*@ functions.
  , atYear
  , atMonthOfYear
  , atDayOfMonth
  , atDay
  , atHour
  , atMinute
  , atSecond
  , atTimeOfDay
  , atMidnight
  , atFirstDayOfWeekOnAfter
  -- * Other
  , diffTZTime
  ) where

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.Except (MonadError)
import Data.Fixed (Pico)
import Data.Time
  (CalendarDiffDays(..), Day, DayOfWeek(..), LocalTime(..), NominalDiffTime, TimeOfDay(..),
  addUTCTime, diffUTCTime, getCurrentTime, midnight, secondsToNominalDiffTime)
import Data.Time qualified as Time
import Data.Time.Calendar.Compat
  (DayOfMonth, MonthOfYear, Year, firstDayOfWeekOnAfter, pattern YearMonthDay)
import Data.Time.TZInfo
import Data.Time.TZTime.Internal as Internal

-- $setup
-- >>> import Data.Function ((&))
-- >>> import Data.Time.TZTime.QQ (tz)
-- >>> import Data.Time

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- | Returns the current time with the local time zone information
-- based on the @TZ@ and @TZDIR@ environment variables.
--
-- See @tzset(3)@ for details, but basically:
--
-- * If @TZ@ environment variable is unset, we use @\/etc\/localtime@.
-- * If @TZ@ is set, but empty, we use `utc`.
-- * If @TZ@ is set and not empty, we use `loadFromSystem` to read that file.
getCurrentTZTime :: IO TZTime
getCurrentTZTime = do
  tzi <- getCurrentTZInfo
  utcNow <- getCurrentTime
  pure $ fromUTC tzi utcNow

----------------------------------------------------------------------------
-- Modifying a TZTime
----------------------------------------------------------------------------

-- | If this local time happens to be on an overlap,
-- switch to the earliest of the two offsets.
--
-- >>> atEarliestOffset [tz|2022-11-06 01:30:00 -06:00 [America/Winnipeg]|]
-- 2022-11-06 01:30:00 -05:00 [America/Winnipeg]
atEarliestOffset :: TZTime -> TZTime
atEarliestOffset tzt =
  case fromLocalTimeStrict (tzTimeTZInfo tzt) (tzTimeLocalTime tzt) of
    Left (TZOverlap _ earliest _) -> earliest
    _ -> tzt

-- | If this local time happens to be on an overlap,
-- switch to the latest of the two offsets.
--
-- >>> atLatestOffset [tz|2022-11-06 01:30:00 -05:00 [America/Winnipeg]|]
-- 2022-11-06 01:30:00 -06:00 [America/Winnipeg]
atLatestOffset :: TZTime -> TZTime
atLatestOffset tzt =
  case fromLocalTimeStrict (tzTimeTZInfo tzt) (tzTimeLocalTime tzt) of
    Left (TZOverlap _ _ latest) -> latest
    _ -> tzt

-- | Changes the time to the earliest time possible on that day.
--
-- This is usually 00:00, but, if, on that day:
--
-- * the clocks are turned, for example, from 23:59 to 01:00 and midnight is skipped,
--   this will return 01:00.
-- * the clocks are turned, for example, from 01:00 to 00:00 and midnight happens twice,
--   this will return the first occurrence (i.e. midnight at the earliest offset).
atStartOfDay :: TZTime -> TZTime
atStartOfDay tzt =
  case Internal.modifyLocalTimeLine atMidnight tzt of
    Right result -> result
    Left (TZGap _ _ after) -> after
    Left (TZOverlap _ atEarliestOffset _) -> atEarliestOffset

----------------------------------------------------------------------------
-- Adding seconds/minutes/hours.
----------------------------------------------------------------------------

{- | Adds the given amount of seconds

>>> [tz|2022-03-04 10:15:00 [Europe/Rome]|] & addTime (hours 2 + minutes 20)
2022-03-04 12:35:00 +01:00 [Europe/Rome]
-}
addTime :: NominalDiffTime -> TZTime -> TZTime
addTime = Internal.modifyUniversalTimeLine . addUTCTime

-- | A standard hour of 3600 seconds.
hours :: Pico -> NominalDiffTime
hours h = minutes (h * 60)

-- | A standard minute of 60 seconds.
minutes :: Pico -> NominalDiffTime
minutes m = seconds (m * 60)

seconds :: Pico -> NominalDiffTime
seconds = secondsToNominalDiffTime

----------------------------------------------------------------------------
-- Local time-line.
----------------------------------------------------------------------------

{- |
Modifies the date/time on the local time-line.

The result may:

* Land on a "gap", e.g. when the clocks are set forward in spring and a local time is skipped.
  When this happens, we shift the time forward by the duration of the gap.

    For example, on the 13th, the clocks skip one hour,
    from 01:59 (at the -06:00 offset) straight to 03:00 (at the -05:00 offset):

    >>> [tz|2022-03-12 02:15:00 -06:00 [America/Winnipeg]|] & modifyLocal (addCalendarClip (calendarDays 1))
    2022-03-13 03:15:00 -05:00 [America/Winnipeg]

* Land on an "overlap", e.g. when the clocks are set back in autumn and a local time happens twice.
  When this happens, we attempt to preserve the offset of the original `TZTime`.
  This ensures that @modifyLocal id == id@.
  If this is not possible, use the earliest of the two offsets.

    For example, on the 6th, the clocks are set back one hour,
    from 01:59 (at the -05:00 offset) back to 01:00 (at the -06:00 offset).
    This means the time 01:15 happens twice, first at -05:00 and then again at -06:00.

    >>> [tz|2022-11-05 01:15:00 -05:00 [America/Winnipeg]|] & modifyLocal (addCalendarClip (calendarDays 1))
    2022-11-06 01:15:00 -05:00 [America/Winnipeg]

    >>> [tz|2022-11-07 01:15:00 -06:00 [America/Winnipeg]|] & modifyLocal (addCalendarClip (calendarDays -1))
    2022-11-06 01:15:00 -06:00 [America/Winnipeg]

This behaviour should be suitable for most use cases.

Note: @modifyLocal (g . f)@ may not always be equivalent to
@modifyLocal g . modifyLocal f@.

If @modifyLocal f@ lands on a gap or an overlap, the time will be corrected as described above;
but there's a chance @modifyLocal (g . f)@ would skip right over
the gap/overlap and no correction is needed.
As a rule of thumb, apply all modifications to the local time-line in one go.

>>> import Control.Arrow ((>>>))
>>> :{
[tz|2022-03-04 10:15:00 +01:00 [Europe/Rome]|]
  & modifyLocal (
      addCalendarClip (calendarMonths 2 <> calendarDays 3) >>>
      atFirstDayOfWeekOnAfter Wednesday >>>
      atMidnight
    )
:}
2022-05-11 00:00:00 +02:00 [Europe/Rome]

-}
modifyLocal :: (LocalTime -> LocalTime) -> TZTime -> TZTime
modifyLocal f tzt =
  case modifyLocalStrict f tzt of
    Right result -> result
    Left (TZGap _ _ after) -> after
    Left (TZOverlap _ atEarliestOffset atLatestOffset)
      | tzTimeOffset atLatestOffset == tzTimeOffset tzt -> atLatestOffset
      | otherwise -> atEarliestOffset

-- | Similar to `modifyLocal`, but returns a `TZError`
-- if the result lands in a gap/overlap.
modifyLocalStrict :: MonadError TZError m => (LocalTime -> LocalTime) -> TZTime -> m TZTime
modifyLocalStrict = Internal.modifyLocalTimeLine

-- | Similar to `modifyLocal`, but throws a `TZError` in `MonadThrow`
-- if the result lands in a gap/overlap.
modifyLocalThrow :: MonadThrow m => (LocalTime -> LocalTime) -> TZTime -> m TZTime
modifyLocalThrow f =
  either throwM pure . modifyLocalStrict f

----------------------------------------------------------------------------
-- Adding days/weeks/months/years.
----------------------------------------------------------------------------

-- | Add the given number of months first and then the given number of days,
-- using the proleptic Gregorian calendar.
--
-- When adding months, days past the last day of the month are clipped to the last day.
-- For instance, 2005-01-30 + 1 month = 2005-02-28.
addCalendarClip :: CalendarDiffDays -> LocalTime -> LocalTime
addCalendarClip cdd lt = lt
  { localDay = Time.addGregorianDurationClip cdd $ localDay lt
  }

-- | Add the given number of months first and then the given number of days.
-- using the proleptic Gregorian calendar.
--
-- When adding months, days past the last day of the month roll over to the next month.
-- For instance, 2005-01-30 + 1 month = 2005-03-02.
addCalendarRollOver :: CalendarDiffDays -> LocalTime -> LocalTime
addCalendarRollOver cdd lt = lt
  { localDay = Time.addGregorianDurationRollOver cdd $ localDay lt
  }

calendarDays :: Integer -> CalendarDiffDays
calendarDays n = CalendarDiffDays 0 n

calendarWeeks :: Integer -> CalendarDiffDays
calendarWeeks n = CalendarDiffDays 0 (n * 7)

calendarMonths :: Integer -> CalendarDiffDays
calendarMonths n = CalendarDiffDays n 0

calendarYears :: Integer -> CalendarDiffDays
calendarYears n = CalendarDiffDays (n * 12) 0

----------------------------------------------------------------------------
-- Setting date/time components.
----------------------------------------------------------------------------

-- | Sets the year using the proleptic Gregorian calendar.
atYear :: Year -> LocalTime -> LocalTime
atYear y lt =
  let YearMonthDay _ moy dom = localDay lt
  in  lt { localDay = YearMonthDay y moy dom }

-- | Sets the month using the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range.
atMonthOfYear :: MonthOfYear -> LocalTime -> LocalTime
atMonthOfYear moy lt =
  let YearMonthDay y _ dom = localDay lt
  in  lt { localDay = YearMonthDay y moy dom }

-- | Sets the day of month using the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range.
atDayOfMonth :: DayOfMonth -> LocalTime -> LocalTime
atDayOfMonth dom lt =
  let YearMonthDay y m _ = localDay lt
  in  lt { localDay = YearMonthDay y m dom }

-- | Sets the day.
atDay :: Day -> LocalTime -> LocalTime
atDay day lt = lt { localDay = day }

atHour :: Int -> LocalTime -> LocalTime
atHour h lt = lt
  { localTimeOfDay = (localTimeOfDay lt) { todHour = h }
  }

atMinute :: Int -> LocalTime -> LocalTime
atMinute m lt = lt
  { localTimeOfDay = (localTimeOfDay lt) { todMin = m }
  }

atSecond :: Pico -> LocalTime -> LocalTime
atSecond s lt = lt
  { localTimeOfDay = (localTimeOfDay lt) { todSec = s }
  }

atTimeOfDay :: TimeOfDay -> LocalTime -> LocalTime
atTimeOfDay tod lt = lt { localTimeOfDay = tod }

-- | Sets the time to 00:00.
atMidnight :: LocalTime -> LocalTime
atMidnight = atTimeOfDay midnight

-- | Moves the date to the next given `DayOfWeek`.
-- If the current date is already a match, then the current date is returned unmodified.
--
-- >>> tzt = [tz|2022-02-24 10:00:00 [Europe/London]|]
-- >>> tzt & modifyLocal (atFirstDayOfWeekOnAfter Thursday)
-- 2022-02-24 10:00:00 +00:00 [Europe/London]
-- >>> tzt & modifyLocal (atFirstDayOfWeekOnAfter Wednesday)
-- 2022-03-02 10:00:00 +00:00 [Europe/London]
atFirstDayOfWeekOnAfter :: DayOfWeek -> LocalTime -> LocalTime
atFirstDayOfWeekOnAfter dow lt = lt
  { localDay = firstDayOfWeekOnAfter dow $ localDay lt
  }

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

-- | Calculate the difference in seconds between two points in time.
diffTZTime :: TZTime -> TZTime -> NominalDiffTime
diffTZTime tzt1 tzt2 =
  diffUTCTime (toUTC tzt1) (toUTC tzt2)
