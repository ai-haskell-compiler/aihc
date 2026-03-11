 {-| This is a minimal Haskell library to display duration.

  @
  > let duration = 2 * ms + 3 * oneSecond + 2 * minute + 33*day + 2*year
  > humanReadableDuration duration
  "2 years 33 days 2 min 3s 2ms"
  > getYears duration
  2
  > getDays duration
  763
  > getMs duration
  65923323002
  @

-}
module Data.Duration
    ( humanReadableDuration
    , humanReadableDuration'
    , approximativeDuration
    , Seconds
    -- durations
    , ms
    , oneSecond
    , minute
    , hour
    , day
    , year
    -- Retrieve
    , getMs
    , getSeconds
    , getMinutes
    , getHours
    , getDays
    , getYears
    ) where

import           Data.Fixed (Fixed (..), Micro, div', mod')

type Seconds = Micro

{- | `humanReadableDuration` take some time in micro-second precision and render a human readable duration.

>>> let duration = 2 * ms + 3 * oneSecond + 2 * minute + 33*day + 2*year
>>> duration
65923323.002000
>>> humanReadableDuration duration
"2 years 33 days 2 min 3s 2ms"
-}
humanReadableDuration :: Seconds -> String
humanReadableDuration n
  | n < oneSecond = let mi = getMs      n in if mi > 0 then show mi ++ "ms" else ""
  | n < minute = let s  = getSeconds n in if s  > 0 then show s  ++ "s " ++ humanReadableDuration (n `mod'` oneSecond) else ""
  | n < hour   = let m  = getMinutes n in if m  > 0 then show m  ++ " min " ++ humanReadableDuration (n `mod'` minute) else ""
  | n < day    = let h  = getHours   n in if h  > 0 then show h  ++ " hours " ++ humanReadableDuration (n `mod'` hour) else ""
  | n < year   = let d  = getDays    n in if d  > 0 then show d  ++ " days " ++ humanReadableDuration (n `mod'` day) else ""
  | otherwise  = let y  = getYears   n in if y  > 0 then show y  ++ " years " ++ humanReadableDuration (n `mod'` year) else ""


{- | `humanReadableDuration` take some time in micro-second precision and render a human readable duration.

>>> let duration = 2 * ms + 3 * oneSecond + 2 * minute + 33*day + 2*year
>>> duration
65923323.002000
>>> approximativeDuration duration
"2 years"
>>> let duration = 2 * ms + 3 * oneSecond + 2 * minute + 33*day
>>> approximativeDuration duration
"33 days"
>>> let duration = 2 * ms + 3 * oneSecond + 280 * minute
>>> approximativeDuration duration
"4 hours"
>>> let duration = 2 * ms + 3 * oneSecond + 22 * minute
>>> approximativeDuration duration
"22 min"
>>> let duration = 2 * ms + 3 * oneSecond
>>> approximativeDuration duration
"3s"
>>> let duration = 12 * ms
>>> approximativeDuration duration
"12ms"
-}
approximativeDuration :: Micro -> String
approximativeDuration n
  | n < oneSecond = let mi = getMs   n in show mi ++ "ms"
  | n < minute = let s  = getSeconds n in show s  ++ "s"
  | n < hour   = let m  = getMinutes n in show m  ++ " min"
  | n < day    = let h  = getHours   n in show h  ++ " hours"
  | n < year   = let d  = getDays    n in show d  ++ " days"
  | otherwise  = let y  = getYears   n in show y  ++ " years"

-- | Wrapper around any `Real` input, which works for `DiffTime` and
-- `NominalDiffTime` from the time library, or a `Double` of seconds.
--
-- >>> import Data.Time.Clock
-- >>> humanReadableDuration' (secondsToDiffTime 10)
-- "10s "
humanReadableDuration' :: Real a => a -> String
humanReadableDuration' = humanReadableDuration . realToFrac

--------------------------------------------------------------------------------
-- Durations
--------------------------------------------------------------------------------

-- | one millisecond (@0.001@)
--
-- >>> ms
-- 0.001000
-- >>> 1000 * ms
-- 1.000000
ms :: Seconds
ms = MkFixed 1000

-- | one second (@1@)
--
-- >>> oneSecond / ms
-- 1000.000000
-- >>> oneSecond
-- 1.000000
oneSecond :: Seconds
oneSecond = 1000 * ms

-- | number of seconds in one minute
--
-- >>> minute / oneSecond
-- 60.000000
-- >>> minute / ms
-- 60000.000000
minute :: Seconds
minute = 60 * oneSecond

-- | number of seconds in one hour
--
-- >>> hour / minute
-- 60.000000
-- >>> hour / oneSecond
-- 3600.000000
hour :: Seconds
hour = 60 * minute

-- | number of seconds in one day
--
-- >>> day / hour
-- 24.000000
-- >>> day / oneSecond
-- 86400.000000
day :: Seconds
day = 24 * hour

-- | number of seconds in one year
--
-- >>> year / day
-- 365.000000
year :: Seconds
year = 365 * day

--------------------------------------------------------------------------------
-- Retrieve some durations
--------------------------------------------------------------------------------
-- | number of milli seconds given a duration in micro seconds
--
-- >>> getMs 1
-- 1000
-- >>> getMs 1.618033
-- 1618
getMs :: Seconds -> Integer
getMs n = n `div'` ms

-- | number of seconds given a duration in micro seconds
--
-- >>> getSeconds 1
-- 1
-- >>> getSeconds 1.618033
-- 1
getSeconds :: Seconds -> Integer
getSeconds n = n `div'` oneSecond

-- | number of minutes given a duration in micro seconds
--
-- >>> getMinutes 60
-- 1
-- >>> getMinutes 59
-- 0
getMinutes :: Seconds -> Integer
getMinutes n = n `div'` minute

-- | number of hours given a duration in micro seconds
--
-- >>> getHours 3600
-- 1
-- >>> getHours (60 * minute)
-- 1
-- >>> getHours (2 * day)
-- 48
getHours :: Seconds -> Integer
getHours n = n `div'` hour

-- | number of days given a duration in micro seconds
--
-- >>> getDays (10 * day)
-- 10
-- >>> getDays (240 * hour)
-- 10
getDays :: Seconds -> Integer
getDays n = n `div'` day

-- | number of years given a duration in micro seconds
--
-- >>> getYears (720 * day)
-- 1
-- >>> getYears (740 * day)
-- 2
getYears :: Seconds -> Integer
getYears n = n `div'` year
