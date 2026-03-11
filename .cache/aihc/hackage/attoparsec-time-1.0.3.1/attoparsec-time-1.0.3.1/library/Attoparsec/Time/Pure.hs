module Attoparsec.Time.Pure where

import Attoparsec.Time.Prelude
import qualified Data.ByteString as A

{-# INLINE timeZone #-}
timeZone :: Bool -> Int -> Int -> TimeZone
timeZone positive hour minute =
  minutesToTimeZone
    $ bool negate id positive
    $ hour
    * 60
    + minute

{-# INLINE day #-}
day :: Int -> Int -> Int -> Day
day y m d =
  fromGregorian (fromIntegral y) m d

{-# INLINE timeOfDay #-}
timeOfDay :: Int -> Int -> Pico -> TimeOfDay
timeOfDay =
  TimeOfDay

{-# INLINE zonedTime #-}
zonedTime :: Day -> TimeOfDay -> TimeZone -> ZonedTime
zonedTime day tod tz =
  ZonedTime (LocalTime day tod) tz

{-# INLINE utcTimeFromDayAndTimeOfDay #-}
utcTimeFromDayAndTimeOfDay :: Day -> TimeOfDay -> TimeZone -> UTCTime
utcTimeFromDayAndTimeOfDay day tod tz =
  zonedTimeToUTC (zonedTime day tod tz)

{-# INLINE utcTimeFromComponents #-}
utcTimeFromComponents :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> TimeZone -> UTCTime
utcTimeFromComponents year month day hour minute second millisecond timeZone =
  undefined

{-# INLINE decimalFromBytes #-}
decimalFromBytes :: (Integral decimal) => A.ByteString -> decimal
decimalFromBytes =
  A.foldl' step 0
  where
    step a b =
      a * 10 + fromIntegral b - 48

{-# INLINE word8IsAsciiDigit #-}
word8IsAsciiDigit :: Word8 -> Bool
word8IsAsciiDigit w =
  w - 48 <= 9

{-# INLINE word8IsAsciiAlpha #-}
word8IsAsciiAlpha :: Word8 -> Bool
word8IsAsciiAlpha x =
  (x - 97 <= 25) || (x - 65 <= 25)
