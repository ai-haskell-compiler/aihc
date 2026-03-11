{-# LANGUAGE BangPatterns #-}
module Data.Time.TimeSpan
  ( TimeSpan
  , milliseconds, seconds, minutes, hours, days, weeks
  , toMicroseconds, toMilliseconds, toSeconds, toMinutes, toHours, toDays, toWeeks
  , absTS, multiplyTS
  , diffUTCTimeTS, addUTCTimeTS, subUTCTimeTS
  , sleepTS, timeoutTS
  , timeAction
  )
where

import Control.Concurrent
import Data.Time
import System.CPUTime
import System.Timeout

-- | An abstract timespan. Use the provided smart constructors to create
-- a meaningful timespan. Note that on first sight a `Num` instance might
-- seem desirable, but this would defeat the purpose of having transparent
-- and explicitly constructed timespans due to `fromInteger`.
newtype TimeSpan
  = TimeSpan { unTimeSpan :: Double } -- as milliseconds
  deriving (Show, Eq, Ord)

instance Semigroup TimeSpan where
    (<>) = mappend

-- | An empty `TimeSpan` is 0, and `mappend` is defined as addition
instance Monoid TimeSpan where
    mempty = TimeSpan 0
    mappend (TimeSpan a) (TimeSpan b) = TimeSpan (a + b)

milliseconds :: Double -> TimeSpan
milliseconds = TimeSpan

seconds :: Double -> TimeSpan
seconds = milliseconds . (* 1000)

minutes :: Double -> TimeSpan
minutes = seconds . (* 60)

hours :: Double -> TimeSpan
hours = minutes . (* 60)

days :: Double -> TimeSpan
days = hours . (* 24)

weeks :: Double -> TimeSpan
weeks = days . (* 7)

toMicroseconds :: TimeSpan -> Double
toMicroseconds = (* 1000) . toMilliseconds

toMilliseconds :: TimeSpan -> Double
toMilliseconds = unTimeSpan

toSeconds :: TimeSpan -> Double
toSeconds = (/1000) . toMilliseconds

toMinutes :: TimeSpan -> Double
toMinutes = (/60) . toSeconds

toHours :: TimeSpan -> Double
toHours = (/60) . toMinutes

toDays :: TimeSpan -> Double
toDays = (/24) . toHours

toWeeks :: TimeSpan -> Double
toWeeks = (/7) . toDays

absTS :: TimeSpan -> TimeSpan
absTS (TimeSpan x) = TimeSpan (abs x)

multiplyTS :: TimeSpan -> Double -> TimeSpan
multiplyTS (TimeSpan x) fact = TimeSpan (fact * x)

diffUTCTimeTS :: UTCTime -> UTCTime -> TimeSpan
diffUTCTimeTS a b = seconds $ fromRational $ toRational $ diffUTCTime a b

addUTCTimeTS :: TimeSpan -> UTCTime -> UTCTime
addUTCTimeTS a = addUTCTime (fromRational $ toRational $ toSeconds a)

subUTCTimeTS :: TimeSpan -> UTCTime -> UTCTime
subUTCTimeTS a = addUTCTime ((-1) * fromRational (toRational $ toSeconds a))

sleepTS :: TimeSpan -> IO ()
sleepTS ts = threadDelay (round $ toMicroseconds ts)

timeoutTS :: TimeSpan -> IO a -> IO (Maybe a)
timeoutTS ts = timeout (round $ toMicroseconds ts)

timeAction :: IO a -> IO (TimeSpan, a)
timeAction action =
    do !t1 <- getCPUTime
       !a <- action
       !t2 <- getCPUTime
       let secs = fromIntegral (t2-t1) * 1e-12
       return (seconds secs, a)
