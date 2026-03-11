--------------------------------------------------------------------------------
-- |
-- Module : Data.DotNet.TimeSpan.Internal
-- Copyright : (C) 2016 Yorick Laupa
-- License : (see the file LICENSE)
--
-- Maintainer : Yorick Laupa <yo.eight@gmail.com>
-- Stability : provisional
-- Portability : non-portable
--
-- .NET TimeSpan implemented in Haskell.
--------------------------------------------------------------------------------
module Data.DotNet.TimeSpan.Internal where

--------------------------------------------------------------------------------
import Data.Int
import Data.Monoid
import Prelude

--------------------------------------------------------------------------------
-- | .NET TimeSpan: Represents a time interval.
newtype TimeSpan = TimeSpan Int64 deriving (Eq, Ord)

--------------------------------------------------------------------------------
instance Show TimeSpan where
    show = timeSpanString

--------------------------------------------------------------------------------
millisPerSecond :: Int64
millisPerSecond = 1000

--------------------------------------------------------------------------------
millisPerMinute :: Int64
millisPerMinute = millisPerSecond * 60

--------------------------------------------------------------------------------
millisPerHour :: Int64
millisPerHour = millisPerMinute * 60

--------------------------------------------------------------------------------
millisPerDay :: Int64
millisPerDay = millisPerHour * 24

--------------------------------------------------------------------------------
ticksPerMillisecond :: Int64
ticksPerMillisecond = 10000

--------------------------------------------------------------------------------
ticksPerSecond :: Int64
ticksPerSecond = ticksPerMillisecond * 1000

--------------------------------------------------------------------------------
ticksPerMinute :: Int64
ticksPerMinute = ticksPerSecond * 60

--------------------------------------------------------------------------------
ticksPerHour :: Int64
ticksPerHour = ticksPerMinute * 60

--------------------------------------------------------------------------------
ticksPerDay :: Int64
ticksPerDay = ticksPerHour * 24

--------------------------------------------------------------------------------
daysPerTick :: Double
daysPerTick = 1 / (realToFrac ticksPerDay)

--------------------------------------------------------------------------------
hoursPerTick :: Double
hoursPerTick = 1 / (realToFrac ticksPerHour)

--------------------------------------------------------------------------------
minutesPerTick :: Double
minutesPerTick = 1 / (realToFrac ticksPerMinute)

--------------------------------------------------------------------------------
secondsPerTick :: Double
secondsPerTick = 1 / (realToFrac ticksPerSecond)

--------------------------------------------------------------------------------
millisPerTick :: Double
millisPerTick = 1 / (realToFrac ticksPerMillisecond)

--------------------------------------------------------------------------------
maxMillis :: Int64
maxMillis =
    truncate
    (((realToFrac (maxBound :: Int64) :: Double)
      / realToFrac ticksPerMillisecond) :: Double)

--------------------------------------------------------------------------------
minMillis :: Int64
minMillis =
    truncate
    (((realToFrac (minBound :: Int64) :: Double)
      / realToFrac ticksPerMillisecond) :: Double)

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to the specified
--   number of ticks.
timeSpanTicks :: Int64 -> TimeSpan
timeSpanTicks = TimeSpan

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to a specified number
--   of hours, minutes, and seconds.
timeSpanHoursMinsSecs :: Int64 -> Int64 -> Int64 -> TimeSpan
timeSpanHoursMinsSecs hh mm ss = TimeSpan $ totalSecs * ticksPerSecond
  where
    totalSecs = (hh * 3600) + (mm * 60) + ss

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to a specified number
--   of days, hours, minutes, and seconds.
timeSpanDaysHoursMinsSecs :: Int64 -> Int64 -> Int64 -> Int64 -> TimeSpan
timeSpanDaysHoursMinsSecs dd hh mm ss =
    timeSpanDaysHoursMinsSecsMillis dd hh mm ss 0

--------------------------------------------------------------------------------
-- | Initializes a new instance of the TimeSpan structure to a specified number
--   of days, hours, minutes, seconds, and milliseconds.
timeSpanDaysHoursMinsSecsMillis :: Int64
                                -> Int64
                                -> Int64
                                -> Int64
                                -> Int64
                                -> TimeSpan
timeSpanDaysHoursMinsSecsMillis dd hh mm ss ms =
    TimeSpan $ _totalMillis * ticksPerMillisecond
  where
    _totalMillis = ((dd * 3600 * 24) +
                    (hh * 3600)       +
                    (mm * 60)         +
                    ss) * 1000 + ms

--------------------------------------------------------------------------------
-- | Gets the number of ticks that represent the value of the current 'TimeSpan'
--   structure.
ticks :: TimeSpan -> Int64
ticks (TimeSpan i) = i

--------------------------------------------------------------------------------
-- | Gets the days component of the time interval represented by the current
--   'TimeSpan' structure.
days :: TimeSpan -> Int64
days (TimeSpan i) = truncate $ (realToFrac i :: Double) /
                               (realToFrac ticksPerDay)

--------------------------------------------------------------------------------
-- | Gets the hours component of the time interval represented by the current
--   'TimeSpan' structure.
hours :: TimeSpan -> Int64
hours (TimeSpan i) = mod (truncate $
                          (realToFrac i :: Double) /
                          (realToFrac ticksPerHour)) 24

--------------------------------------------------------------------------------
-- | Gets the minutes component of the time interval represented by the current
--   'TimeSpan' structure.
minutes :: TimeSpan -> Int64
minutes (TimeSpan i) = mod (truncate $
                            (realToFrac i :: Double) /
                            (realToFrac ticksPerMinute)) 60

--------------------------------------------------------------------------------
-- | Gets the seconds component of the time interval represented by the current
--   'TimeSpan' structure.
seconds :: TimeSpan -> Int64
seconds (TimeSpan i) = mod (truncate $
                            (realToFrac i :: Double) /
                            (realToFrac ticksPerSecond)) 60

--------------------------------------------------------------------------------
-- | Gets the milliseconds component of the time interval represented by the
--   current 'TimeSpan' structure.
millis :: TimeSpan -> Int64
millis (TimeSpan i) = mod (truncate $
                           (realToFrac i :: Double) /
                           (realToFrac ticksPerMillisecond)) 1000

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of seconds, where
--   the specification is accurate to the nearest millisecond.
fromSeconds :: Double -> TimeSpan
fromSeconds i = interval i millisPerSecond

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of minutes, where
--   the specification is accurate to the nearest millisecond.
fromMinutes :: Double -> TimeSpan
fromMinutes i = interval i millisPerMinute

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of hours, where the
--   specification is accurate to the nearest millisecond.
fromHours :: Double -> TimeSpan
fromHours i = interval i millisPerHour

--------------------------------------------------------------------------------
-- | Returns a 'TimeSpan' that represents a specified number of days, where the
--   specification is accurate to the nearest millisecond.
fromDays :: Double -> TimeSpan
fromDays i = interval i millisPerDay

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional days.
totalDays :: TimeSpan -> Double
totalDays (TimeSpan i) = (realToFrac i) * daysPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional hours.
totalHours :: TimeSpan -> Double
totalHours (TimeSpan i) = (realToFrac i) * hoursPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional minutes.
totalMinutes :: TimeSpan -> Double
totalMinutes (TimeSpan i) = (realToFrac i) * minutesPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional seconds.
totalSeconds :: TimeSpan -> Double
totalSeconds (TimeSpan i) = (realToFrac i) * secondsPerTick

--------------------------------------------------------------------------------
-- | Gets the value of the current 'TimeSpan' structure expressed in whole and
--   fractional milliseconds.
totalMillis :: TimeSpan -> Double
totalMillis (TimeSpan i) =
    let tmp = (realToFrac i) * millisPerTick in
    if tmp > (realToFrac maxMillis) then realToFrac maxMillis
    else if tmp < (realToFrac minMillis) then realToFrac minMillis
         else tmp

--------------------------------------------------------------------------------
data FormatLiteral = Positive | Negative

--------------------------------------------------------------------------------
padded :: Int -> a -> [a] -> [a]
padded n p xs = replicate diff p ++ xs
  where
    len_xs = length xs
    diff   = n - len_xs

--------------------------------------------------------------------------------
timeSpanString :: TimeSpan -> String
timeSpanString (TimeSpan _ticks) =
    start    <>
    genDay   <>
    genHours <>
    genMins  <>
    genSecs  <>
    genFract

  where
    ticksPerHourD   = realToFrac ticksPerHour   :: Double
    ticksPerDayD    = realToFrac ticksPerDay    :: Double
    ticksPerMinuteD = realToFrac ticksPerMinute :: Double
    ticksPerSecondD = realToFrac ticksPerSecond :: Double

    day :: Int64
    day = truncate $ realToFrac _ticks / ticksPerDayD

    time = _ticks `mod` ticksPerDay

    cday  = if _ticks < 0 then negate day else day
    ctime = if _ticks < 0 then negate time else time

    _hours :: Int64
    _hours = mod (truncate (realToFrac ctime / ticksPerHourD)) 24

    mins :: Int64
    mins = mod (truncate (realToFrac ctime / ticksPerMinuteD)) 60

    secs :: Int64
    secs = mod (truncate (realToFrac ctime / ticksPerSecondD)) 60

    fraction :: Int64
    fraction = ctime `mod` ticksPerSecond

    literal = if _ticks < 0 then Negative else Positive

    start =
        case literal of
            Positive -> ""
            Negative -> "-"

    genDay =
        if cday /= 0
        then show cday <> "."
        else mempty

    genHours = padded 2 '0' (show _hours) <> ":"
    genMins  = padded 2 '0' (show mins)  <> ":"
    genSecs  = padded 2 '0' (show secs)

    genFract =
        if fraction /= 0
        then "." <> padded 7 '0' (show fraction)
        else mempty

--------------------------------------------------------------------------------
interval :: Double -> Int64 -> TimeSpan
interval value scale =
    let tmp     = value * (realToFrac scale)
        _millis = tmp + (if value >= 0 then 0.5 else (-0.5))
        res     = truncate (_millis * (realToFrac ticksPerMillisecond)) in
    TimeSpan res
