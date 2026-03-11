-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Data.Time.TZTime.QQ where

import Data.List (intercalate)
import Data.Time (LocalTime(..), TimeOfDay(..), TimeZone)
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.Internal (TZTime(UnsafeTZTime))
import Data.Time.TZTime.QQ (tz)
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Quote (quoteExp)
import Language.Haskell.TH.TestUtils (tryTestQ, unmockedState)
import Test.Tasty.HUnit

shouldFailWith :: String -> [String] -> Assertion
shouldFailWith input expectedErrorLines = do
  runQ (tryTestQ unmockedState (quoteExp tz input)) >>= \case
    Left err -> err @?= expectedError
    Right exp -> assertFailure $ unlines
      [ "Expected QQ to fail, but it succeeded with: "
      , show exp
      ]
  where
    expectedError = intercalate "\n" expectedErrorLines

unit_parses_valid_unambiguous_times :: Assertion
unit_parses_valid_unambiguous_times = do
  let expected = unsafeFromLocalTime (TZI.fromLabel TZI.Europe__Rome) (LocalTime (YearMonthDay 2022 3 4) (TimeOfDay 10 15 40.123))

  [tz|2022-03-04 10:15:40.123 [Europe/Rome]|] @?= expected
  [tz|2022-03-04 10:15:40.123 +01:00 [Europe/Rome]|] @?= expected

unit_fails_when_time_zone_is_unknown :: Assertion
unit_fails_when_time_zone_is_unknown = do
  "2022-01-01 01:02:03 [Narnia]" `shouldFailWith`
    ["Unknown time zone: 'Narnia'"]

unit_fails_when_time_is_valid_but_offset_is_invalid :: Assertion
unit_fails_when_time_is_valid_but_offset_is_invalid = do
  "2022-03-04 10:15:40.123 +03:00 [Europe/Rome]" `shouldFailWith`
    [ "Invalid offset: +03:00"
    , "      Did you mean any of the following?"
    , "      - 2022-03-04 10:15:40.123 +01:00 [Europe/Rome]"
    ]

unit_fails_when_time_lands_on_a_gap :: Assertion
unit_fails_when_time_lands_on_a_gap =
  "2022-03-13 02:30:00 [America/Winnipeg]" `shouldFailWith`
    [ "Invalid time: the clocks are set forward around this time."
    , "      Did you mean any of the following?"
    , "      - 2022-03-13 01:30:00 -06:00 [America/Winnipeg]"
    , "      - 2022-03-13 03:30:00 -05:00 [America/Winnipeg]"
    ]

unit_is_ambiguous_when_time_lands_on_an_overlap :: Assertion
unit_is_ambiguous_when_time_lands_on_an_overlap =
  "2022-11-06 01:30:00 [America/Winnipeg]" `shouldFailWith`
    [ "Ambiguous time: please specify an offset."
    , "      Did you mean any of the following?"
    , "      - 2022-11-06 01:30:00 -05:00 [America/Winnipeg]"
    , "      - 2022-11-06 01:30:00 -06:00 [America/Winnipeg]"
    ]

unit_is_not_ambiguous_when_time_lands_on_an_overlap_and_an_offset_is_specified :: Assertion
unit_is_not_ambiguous_when_time_lands_on_an_overlap_and_an_offset_is_specified = do
  [tz|2022-11-06 01:30:00 CDT [America/Winnipeg]|] @?=
    UnsafeTZTime
      (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0))
      (TZI.fromLabel TZI.America__Winnipeg)
      (read @TimeZone "CDT")
  [tz|2022-11-06 01:30:00 CST [America/Winnipeg]|] @?=
    UnsafeTZTime
      (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0))
      (TZI.fromLabel TZI.America__Winnipeg)
      (read @TimeZone "CST")

unit_fails_when_time_lands_on_an_overlap_and_the_offset_is_invalid :: Assertion
unit_fails_when_time_lands_on_an_overlap_and_the_offset_is_invalid = do
  "2022-11-06 01:30:00 +00:00 [America/Winnipeg]" `shouldFailWith`
    [ "Invalid offset: +00:00"
    , "      Did you mean any of the following?"
    , "      - 2022-11-06 01:30:00 -05:00 [America/Winnipeg]"
    , "      - 2022-11-06 01:30:00 -06:00 [America/Winnipeg]"
    ]

unit_fails_when_theres_extra_input :: Assertion
unit_fails_when_theres_extra_input = do
  "2022-03-04 10:15:40.123 [Europe/Rome] more input" `shouldFailWith`
    ["Failed to parse: '2022-03-04 10:15:40.123 [Europe/Rome] more input'"]

unit_ignores_surrounding_whitespace :: Assertion
unit_ignores_surrounding_whitespace = do
  [tz|   2022-03-04 10:15:40.123 [Europe/Rome]   |] @?= [tz|2022-03-04 10:15:40.123 [Europe/Rome]|]
