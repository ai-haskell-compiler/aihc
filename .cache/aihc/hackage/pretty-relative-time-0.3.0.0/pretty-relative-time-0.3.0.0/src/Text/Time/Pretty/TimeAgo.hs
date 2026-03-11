{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Time.Pretty.TimeAgo
  ( daysAgo,
    daysAgoToDays,
    DaysAgo (..),
    timeAgo,
    timeAgoToDiffTime,
    TimeAgo (..),
  )
where

import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Text.Time.Pretty.Constants

data DaysAgo = DaysAgo
  { daysAgoSign :: Ordering,
    daysAgoYears :: Integer,
    daysAgoMonths :: Integer,
    daysAgoWeeks :: Integer,
    daysAgoDays :: Integer
  }
  deriving (Show, Eq, Generic)

instance Validity DaysAgo where
  validate da@DaysAgo {..} =
    mconcat
      [ genericValidate da,
        check
          ( case daysAgoSign of
              EQ ->
                and
                  [ daysAgoDays == 0,
                    daysAgoWeeks == 0,
                    daysAgoMonths == 0,
                    daysAgoYears == 0
                  ]
              _ ->
                any
                  (> 0)
                  [daysAgoDays, daysAgoWeeks, daysAgoMonths, daysAgoYears]
          )
          "the sign makes sense",
        check (daysAgoYears >= 0) "years are positive",
        check
          ( daysAgoDays + daysPerWeek * daysAgoWeeks
              + approximateDaysPerMonth * daysAgoMonths
              < approximateDaysPerYear
          )
          "days, weeks and months do not sum to a year",
        check (daysAgoMonths < 12) "months < 12",
        check (daysAgoMonths >= 0) "months are positive",
        check
          (daysAgoDays + daysPerWeek * daysAgoWeeks < approximateDaysPerMonth)
          "days and weeks do not sum to a month",
        check (daysAgoWeeks < 5) "weeks < 5",
        check (daysAgoWeeks >= 0) "weeks are positive",
        check (daysAgoDays < 7) "days < 7",
        check (daysAgoDays >= 0) "days are positive"
      ]

daysAgo :: Integer -> DaysAgo
daysAgo i = DaysAgo {..}
  where
    totalDays = abs i
    daysAgoSign = compare i 0
    daysAgoYears = totalDays `div` approximateDaysPerYear
    daysLeftAfterYears = totalDays - daysAgoYears * approximateDaysPerYear
    daysAgoMonths = daysLeftAfterYears `div` approximateDaysPerMonth
    daysLeftAfterMonths =
      daysLeftAfterYears - daysAgoMonths * approximateDaysPerMonth
    daysAgoWeeks = daysLeftAfterMonths `div` daysPerWeek
    daysLeftAfterWeeks = daysLeftAfterMonths - daysAgoWeeks * daysPerWeek
    daysAgoDays = daysLeftAfterWeeks

daysAgoToDays :: DaysAgo -> Integer
daysAgoToDays DaysAgo {..} =
  ( case daysAgoSign of
      EQ -> const 0
      GT -> id
      LT -> negate
  )
    $ daysAgoDays + daysPerWeek * daysAgoWeeks
      + approximateDaysPerMonth * daysAgoMonths
      + approximateDaysPerYear * daysAgoYears

data TimeAgo = TimeAgo
  { timeAgoSign :: Ordering,
    timeAgoDaysAgo :: DaysAgo,
    timeAgoHours :: Integer,
    timeAgoMinutes :: Integer,
    timeAgoSeconds :: Integer,
    timeAgoPicoSeconds :: Integer
  }
  deriving (Show, Eq, Generic)

instance Validity TimeAgo where
  validate ta@TimeAgo {..} =
    mconcat
      [ genericValidate ta,
        check
          ( case timeAgoSign of
              EQ ->
                and
                  [ daysAgoToDays timeAgoDaysAgo == 0,
                    timeAgoHours == 0,
                    timeAgoMinutes == 0,
                    timeAgoSeconds == 0,
                    timeAgoPicoSeconds == 0
                  ]
              _ ->
                any
                  (> 0)
                  [ daysAgoToDays timeAgoDaysAgo,
                    timeAgoHours,
                    timeAgoMinutes,
                    timeAgoSeconds,
                    timeAgoPicoSeconds
                  ]
          )
          "the sign makes sense",
        check (daysAgoSign timeAgoDaysAgo /= LT) "The days ago are not negative",
        check (timeAgoHours < hoursPerDay) "hours < 24",
        check (timeAgoHours >= 0) "hours are positive",
        check (timeAgoMinutes < minutesPerHour) "minutes < 60",
        check (timeAgoMinutes >= 0) "minutes are positive",
        check (timeAgoSeconds < secondsPerMinute) "seconds < 60",
        check (timeAgoSeconds >= 0) "seconds are positive",
        check (timeAgoPicoSeconds < picoSecondsPerSecond) "picoseconds < 1E12",
        check (timeAgoPicoSeconds >= 0) "picoseconds are positive"
      ]

timeAgo :: NominalDiffTime -> TimeAgo
timeAgo dt = TimeAgo {..}
  where
    timeAgoSign = compare dt 0
    timeAgoPicoSeconds =
      totalPicoSecondsAgo - picoSecondsPerSecond * totalSecondsAgo
    timeAgoSeconds = totalSecondsAgo - secondsPerMinute * totalMinutesAgo
    timeAgoMinutes = totalMinutesAgo - minutesPerHour * totalHoursAgo
    timeAgoHours = totalHoursAgo - hoursPerDay * totalDaysAgo
    timeAgoDaysAgo = daysAgo totalDaysAgo
    totalPicoSecondsAgo =
      floor $ absDt * fromIntegral (picoSecondsPerSecond :: Integer)
    totalSecondsAgo = floor absDt :: Integer
    totalMinutesAgo = floor $ absDt / fromIntegral (secondsPerMinute :: Integer)
    totalHoursAgo =
      floor $
        absDt / fromIntegral (minutesPerHour * secondsPerMinute :: Integer)
    totalDaysAgo =
      floor $
        absDt
          / fromIntegral (hoursPerDay * minutesPerHour * secondsPerMinute :: Integer)
    absDt = abs dt

timeAgoToDiffTime :: TimeAgo -> NominalDiffTime
timeAgoToDiffTime TimeAgo {..} =
  (/ fromIntegral (picoSecondsPerSecond :: Integer)) $
    realToFrac $
      ( case timeAgoSign of
          EQ -> const 0
          GT -> id
          LT -> negate
      )
        ( timeAgoPicoSeconds
            + picoSecondsPerSecond
            * ( timeAgoSeconds
                  + secondsPerMinute
                  * ( timeAgoMinutes
                        + minutesPerHour
                        * (timeAgoHours + hoursPerDay * daysAgoToDays timeAgoDaysAgo)
                    )
              )
        )
