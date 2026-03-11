-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Data.Time.TZTime.FromLocalTime where

import Data.Time (LocalTime(..), TimeOfDay(..), TimeZone(..))
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.Internal (TZTime(UnsafeTZTime))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Utils

unit_fromLocalTimeStrict_at_gaps :: Assertion
unit_fromLocalTimeStrict_at_gaps = do
  -- At 01:59:59 on 2022-03-13, the America/Winnipeg time zone switches
  -- from CST (UTC-6) to CDT (UTC-5).
  -- In other words, the clocks “skip” 1 hour, straight to 03:00:00.
  --
  -- See: https://www.timeanddate.com/time/zone/canada/winnipeg?year=2022
  --
  -- Therefore, the local time "2:15" is invalid.
  -- `fromLocalTimeStrict` should return the local time shifted backwards / forwards
  -- by the duration of the gap (+/- 1 hour).
  let local = LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 2 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  TZT.fromLocalTimeStrict tz local @?= Left
    (TZGap local
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 1 15 0))
        tz cst
      )
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 3 15 0))
        tz cdt
      )
    )

test_fromLocalTimeStrict_at_odd_gaps :: [TestTree]
test_fromLocalTimeStrict_at_odd_gaps =
  [ testCase "Australia/Lord_Howe" do
      -- Australia/Lord_Howe turns the clocks forward 30 mins.
      --
      -- See: https://www.timeanddate.com/time/zone/australia/lord-howe-island?year=2022
      let local = LocalTime (YearMonthDay 2022 10 2) (TimeOfDay 2 7 0)
      let tz = TZI.fromLabel TZI.Australia__Lord_Howe

      TZT.fromLocalTimeStrict tz local @?= Left
        (TZGap local
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2022 10 2) (TimeOfDay 1 37 0))
            tz
            (TimeZone (10 * 60 + 30) False "+1030")
          )
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2022 10 2) (TimeOfDay 2 37 0))
            tz
            (TimeZone (11 * 60) True "+11")
          )
        )

  , testCase "Pacific/Apia" do
      -- On 2011-12-29, at 23:59:59, the island of Samoa switched from UTC-10 to UTC+14.
      -- In other words, the clocks were turned forward 24 hours,
      -- effectively skipping a whole calendar day (from 29 Dec to 31 Dec).
      --
      -- See: https://www.timeanddate.com/time/zone/samoa/apia?year=2011
      let local = LocalTime (YearMonthDay 2011 12 30) (TimeOfDay 10 7 0)
      let tz = TZI.fromLabel TZI.Pacific__Apia

      TZT.fromLocalTimeStrict tz local @?= Left
        (TZGap local
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2011 12 29) (TimeOfDay 10 7 0))
            tz
            (TimeZone (-10 * 60) True "-10")
          )
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2011 12 31) (TimeOfDay 10 7 0))
            tz
            (TimeZone (14 * 60) True "+14")
          )
        )
  ]

unit_fromLocalTimeStrict_at_overlaps :: Assertion
unit_fromLocalTimeStrict_at_overlaps = do
  -- At 01:59:59 on 2022-11-06, the America/Winnipeg time zone switches
  -- from CDT (UTC-5) to CST (UTC-6).
  -- In other words, the clocks are turned back 1 hour, back to 01:00:00.
  --
  -- See: https://www.timeanddate.com/time/zone/canada/winnipeg?year=2022
  --
  -- Therefore, the local time "01:15" happens twice: once at the -5 offset
  -- and again at the -6 offset.
  -- `fromLocalTimeStrict` should return both instants.
  let local = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  TZT.fromLocalTimeStrict tz local @?= Left
    (TZOverlap local
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0))
        tz cdt
      )
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0))
        tz cst
      )
    )

unit_fromLocalTimeStrict_at_odd_overlaps :: Assertion
unit_fromLocalTimeStrict_at_odd_overlaps = do
  -- Australia/Lord_Howe turns the clocks backwards 30 mins.
  --
  -- See: https://www.timeanddate.com/time/zone/australia/lord-howe-island?year=2022
  let local = LocalTime (YearMonthDay 2022 4 3) (TimeOfDay 1 45 0)
  let tz = TZI.fromLabel TZI.Australia__Lord_Howe

  TZT.fromLocalTimeStrict tz local @?= Left
    (TZOverlap local
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 4 3) (TimeOfDay 1 45 0))
        tz
        (TimeZone (11 * 60) True "+11")
      )
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 4 3) (TimeOfDay 1 45 0))
        tz
        (TimeZone (10 * 60 + 30) False "+1030")
      )
    )
