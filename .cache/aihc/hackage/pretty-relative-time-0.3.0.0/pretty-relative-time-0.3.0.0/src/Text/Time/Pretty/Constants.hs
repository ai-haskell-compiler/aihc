module Text.Time.Pretty.Constants
  ( picoSecondsPerSecond,
    secondsPerMinute,
    minutesPerHour,
    hoursPerDay,
    daysPerWeek,
    approximateDaysPerMonth,
    approximateDaysPerYear,
  )
where

picoSecondsPerSecond :: Integral a => a
picoSecondsPerSecond = 10 ^ (12 :: Integer)

secondsPerMinute :: Integral a => a
secondsPerMinute = 60

minutesPerHour :: Integral a => a
minutesPerHour = 60

hoursPerDay :: Integral a => a
hoursPerDay = 24

daysPerWeek :: Integral a => a
daysPerWeek = 7

approximateDaysPerMonth :: Integral a => a
approximateDaysPerMonth = 31

approximateDaysPerYear :: Integral a => a
approximateDaysPerYear = 365
