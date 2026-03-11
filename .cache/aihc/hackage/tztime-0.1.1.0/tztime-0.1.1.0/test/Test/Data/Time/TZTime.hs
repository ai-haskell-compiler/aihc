-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Time.TZTime where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Time (DayOfWeek(..), LocalTime(..), TimeOfDay(..), TimeZone(..))
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.Internal (TZTime(UnsafeTZTime))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Utils

unit_atStartOfDay_at_gaps :: Assertion
unit_atStartOfDay_at_gaps = do
  -- On this date in Havana, at 23:59, the clocks are turned forward 1 hour,
  -- which means midnight never happens.
  --
  -- `atStartOfDay` should return the 01:00, the earliest time possible for that day.
  --
  -- See: https://www.timeanddate.com/time/zone/cuba/havana?year=2022
  let tz = TZI.fromLabel TZI.America__Havana
  let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 10 45 0))
  let result = atStartOfDay tzt

  result @?=
    UnsafeTZTime
      (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 1 0 0))
      tz
      (TimeZone (-4 * 60) True "CDT")

test_atStartOfDay_at_overlaps :: [TestTree]
test_atStartOfDay_at_overlaps =
  [ testCase "America/Havana" do
      -- On this date in Havana, at 01:00, the clocks are turned backwards 1 hour,
      -- which means midnight happens twice.
      --
      -- `atStartOfDay` should return the earliest of the two midnights.
      --
      -- See: https://www.timeanddate.com/time/zone/cuba/havana?year=2022
      let tz = TZI.fromLabel TZI.America__Havana
      let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 10 45 0))
      let result = atStartOfDay tzt

      result @?=
        UnsafeTZTime
          (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 0 0 0))
          tz
          (TimeZone (-4 * 60) True "CDT")

      -- Sanity checks
      -- Make sure this really is an overlap
      assertBool "Expected `result` to land on an overlap" $
        (result & toUTC) < (result & atLatestOffset & toUTC)
      -- This really is the earliest offset
      result @?= atEarliestOffset result

  , testCase "America/St_Johns" do
      -- Up until 2010, in America/St_Johns, the clocks were turned backwards
      -- 1 hour in autumn at 00:01 back to 23:01 the previous day, meaning the **date**
      -- would be moved backwards.
      --
      -- See:
      --   * https://davecturner.github.io/2017/12/27/timezone-curiosities.html
      --   * https://www.timeanddate.com/time/zone/canada/st-johns?year=2009
      let tz = TZI.fromLabel TZI.America__St_Johns
      let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2009 11 1) (TimeOfDay 10 45 0))
      let result = atStartOfDay tzt

      result @?=
        UnsafeTZTime
          (LocalTime (YearMonthDay 2009 11 1) (TimeOfDay 0 0 0))
          tz
          (TimeZone (-2 * 60 - 30) True "NDT")

      -- Sanity checks
      -- Make sure this really is an overlap
      assertBool "Expected `result` to land on an overlap" $
        (result & toUTC) < (result & atLatestOffset & toUTC)
      -- This really is the earliest offset
      result @?= atEarliestOffset result
  ]

unit_addTime_and_diffTZTime_operate_on_the_universal_time_line :: Assertion
unit_addTime_and_diffTZTime_operate_on_the_universal_time_line = do
  -- At 01:59:59 on 2022-11-06, the America/Winnipeg time zone switches
  -- from CDT (UTC-5) to CST (UTC-6).
  -- In other words, the clocks are turned back 1 hour, back to 01:00:00.
  let local = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 0 30 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg
  let tzt = unsafeFromLocalTime tz local

  addTime (hours 1) tzt @?=
    UnsafeTZTime (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0)) tz cdt

  addTime (hours 2) tzt @?=
    UnsafeTZTime (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0)) tz cst

  addTime (hours 3) tzt @?=
    UnsafeTZTime (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 2 30 0)) tz cst

  diffTZTime (addTime (hours 1) tzt) tzt @?= hours 1
  diffTZTime (addTime (hours 2) tzt) tzt @?= hours 2
  diffTZTime (addTime (hours 3) tzt) tzt @?= hours 3

unit_composing_modifiers :: Assertion
unit_composing_modifiers = do
  let tz = TZI.fromLabel TZI.America__Havana
  let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 10 45 0))

  let result =
        tzt
          & modifyLocal (
              addCalendarClip (calendarMonths 2 <> calendarDays 3)
              >>> atFirstDayOfWeekOnAfter Wednesday
              >>> atMidnight
            )
          & atEarliestOffset
          & addTime (minutes 1 + seconds 20)

  result @?= unsafeFromLocalTime tz
    (LocalTime (YearMonthDay 2023 1 11) (TimeOfDay 0 1 20))

unit_addCalendar_zero_equals_id :: Assertion
unit_addCalendar_zero_equals_id = do
  -- With `modifyLocal`, adding 0 days should be equivalent
  -- to the identity function.
  let tz = TZI.fromLabel TZI.America__Winnipeg
  let mkLocalTime hh mm ss = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay hh mm ss)

  check $ unsafeFromLocalTime tz (mkLocalTime 0 0 0)
  check $ unsafeFromLocalTime tz (mkLocalTime 2 0 0)
  check $ fromLocalTimeStrict tz (mkLocalTime 1 0 0) & \(Left (TZOverlap _ earliest _)) -> earliest
  check $ fromLocalTimeStrict tz (mkLocalTime 1 0 0) & \(Left (TZOverlap _ _ latest)) -> latest
  check $ fromLocalTimeStrict tz (mkLocalTime 1 30 0) & \(Left (TZOverlap _ earliest _)) -> earliest
  check $ fromLocalTimeStrict tz (mkLocalTime 1 30 0) & \(Left (TZOverlap _ _ latest)) -> latest
  check $ fromLocalTimeStrict tz (mkLocalTime 1 59 0) & \(Left (TZOverlap _ earliest _)) -> earliest
  check $ fromLocalTimeStrict tz (mkLocalTime 1 59 0) & \(Left (TZOverlap _ _ latest)) -> latest

  where
    check :: TZTime -> Assertion
    check tzt = do
      modifyLocal (addCalendarClip (calendarDays 0)) tzt @?= tzt
