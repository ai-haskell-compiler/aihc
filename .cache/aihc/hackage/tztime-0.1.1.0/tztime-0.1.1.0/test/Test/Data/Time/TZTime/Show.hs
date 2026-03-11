-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Data.Time.TZTime.Show where

import Control.Exception (displayException)
import Data.Bifunctor (first)
import Data.Time (LocalTime(..), TimeOfDay(..))
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.QQ (tz)
import Test.Tasty.HUnit

unit_TZTime :: Assertion
unit_TZTime = do
  show [tz|2022-03-04 02:02:01 +01:00 [Europe/Rome]|] @?=
    "2022-03-04 02:02:01 +01:00 [Europe/Rome]"

unit_TZError :: Assertion
unit_TZError = do
  let tz = TZI.fromLabel TZI.America__Winnipeg

  first displayException
    (TZT.fromLocalTimeStrict tz (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 2 15 0))) @?= Left
    "The local time 2022-03-13 02:15:00 is invalid in the time zone \"America/Winnipeg\"."

  first displayException
    (TZT.fromLocalTimeStrict tz (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0))) @?= Left
    "The local time 2022-11-06 01:15:00 is ambiguous in the time zone \"America/Winnipeg\": it is observed at the offsets -05:00 and -06:00."
