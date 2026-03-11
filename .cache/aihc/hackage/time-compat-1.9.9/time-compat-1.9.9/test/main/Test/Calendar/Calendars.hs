module Test.Calendar.Calendars
  ( testCalendars,
  )
where

import Data.Time.Calendar.Compat
import Data.Time.Calendar.Julian.Compat
import Data.Time.Calendar.WeekDate.Compat
import Test.Calendar.CalendarsRef
import Test.Tasty
import Test.Tasty.HUnit

showers :: [(String, Day -> String)]
showers =
  [ ("MJD", show . toModifiedJulianDay),
    ("Gregorian", showGregorian),
    ("Julian", showJulian),
    ("ISO 8601", showWeekDate)
  ]

days :: [Day]
days = [fromGregorian 0 12 31, fromJulian 1752 9 2, fromGregorian 1752 9 14, fromGregorian 2005 1 23]

testCalendars :: TestTree
testCalendars = testCase "testCalendars" $ assertEqual "" testCalendarsRef $ unlines $ map (\d -> showShowers d) days
  where
    showShowers day = concatMap (\(nm, shower) -> unwords [" ==", nm, shower day]) showers
