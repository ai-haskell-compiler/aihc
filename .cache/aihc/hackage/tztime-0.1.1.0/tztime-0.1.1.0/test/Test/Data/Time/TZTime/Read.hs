-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Data.Time.TZTime.Read where

import Data.Functor ((<&>))
import Data.Time (LocalTime(..), TimeOfDay(..), TimeZone)
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.Internal (TZTime(UnsafeTZTime))
import Data.Time.TZTime.QQ (tz)
import Test.Tasty.HUnit
import Text.Read (readEither)

unit_parses_valid_unambiguous_times :: Assertion
unit_parses_valid_unambiguous_times = do
  let expected = unsafeFromLocalTime (TZI.fromLabel TZI.Europe__Rome) (LocalTime (YearMonthDay 2022 3 4) (TimeOfDay 10 15 40.123))

  reads @TZTime "2022-03-04 10:15:40.123 [Europe/Rome]" @?= [(expected, "")]
  reads @TZTime "2022-03-04 10:15:40.123 +01:00 [Europe/Rome]" @?= [(expected, "")]

unit_fails_when_time_zone_is_unknown :: Assertion
unit_fails_when_time_zone_is_unknown = do
  reads @TZTime "2022-01-01 01:02:03 [Narnia]" @?= []

unit_fails_when_time_is_valid_but_offset_is_invalid :: Assertion
unit_fails_when_time_is_valid_but_offset_is_invalid = do
  reads @TZTime "2022-03-04 10:15:40.123 +03:00 [Europe/Rome]" @?= []

unit_fails_when_time_lands_on_a_gap :: Assertion
unit_fails_when_time_lands_on_a_gap =
  reads @TZTime "2022-03-13 02:30:00 [America/Winnipeg]" @?= []

unit_is_ambiguous_when_time_lands_on_an_overlap :: Assertion
unit_is_ambiguous_when_time_lands_on_an_overlap =
  reads @TZTime "2022-11-06 01:30:00 [America/Winnipeg]" @?=
    [ ( UnsafeTZTime
          (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0))
          (TZI.fromLabel TZI.America__Winnipeg)
          (read @TimeZone "CDT")
      , ""
      )
    , ( UnsafeTZTime
          (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0))
          (TZI.fromLabel TZI.America__Winnipeg)
          (read @TimeZone "CST")
      , ""
      )
    ]

unit_is_not_ambiguous_when_time_lands_on_an_overlap_and_an_offset_is_specified :: Assertion
unit_is_not_ambiguous_when_time_lands_on_an_overlap_and_an_offset_is_specified = do
  reads @TZTime "2022-11-06 01:30:00 CDT [America/Winnipeg]" @?=
    [ ( UnsafeTZTime
          (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0))
          (TZI.fromLabel TZI.America__Winnipeg)
          (read @TimeZone "CDT")
      , ""
      )
    ]
  reads @TZTime "2022-11-06 01:30:00 CST [America/Winnipeg]" @?=
    [ ( UnsafeTZTime
          (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0))
          (TZI.fromLabel TZI.America__Winnipeg)
          (read @TimeZone "CST")
      , ""
      )
    ]

unit_fails_when_time_lands_on_an_overlap_and_the_offset_is_invalid :: Assertion
unit_fails_when_time_lands_on_an_overlap_and_the_offset_is_invalid = do
  reads @TZTime "2022-11-06 01:30:00 +00:00 [America/Winnipeg]" @?= []

unit_returns_remaining_input :: Assertion
unit_returns_remaining_input = do
  (reads @TZTime "2022-03-04 10:15:40.123 [Europe/Rome] more input" <&> snd) @?= [" more input"]
  (reads @TZTime "2022-03-04 10:15:40.123 +01:00 [Europe/Rome] more input" <&> snd) @?= [" more input"]

unit_read_show_roundtrip :: Assertion
unit_read_show_roundtrip = do
  check [tz|2022-03-04 10:15:40.123 [Europe/Rome]|]
  check [tz|2022-03-04 10:15:40 +01:00 [Europe/Rome]|]
  check [tz|2022-11-06 01:30:00 CDT [America/Winnipeg]|]
  check [tz|2022-11-06 01:30:00 CST [America/Winnipeg]|]
  where
    check :: HasCallStack => TZTime -> Assertion
    check tzt = readEither @TZTime (show tzt) @?= Right tzt
