-- |
-- ASCII ByteString Parsers.
module Attoparsec.Time.ByteString
  ( timeOfDayInISO8601,
    dayInISO8601,
    yearAndMonthInISO8601,
    timeZoneInISO8601,
    utcTimeInISO8601,
    diffTime,
    nominalDiffTime,
    hour,
    minute,
    second,
  )
where

import Attoparsec.Time.Prelude hiding (take, takeWhile)
import qualified Attoparsec.Time.Pure as A
import qualified Attoparsec.Time.Validation as B
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as D
import qualified Data.ByteString as C

validated :: (Show a) => B.Validator a -> Parser a -> Parser a
validated validator parser =
  parser >>= \x -> B.run validator (pure x) fail x

sign :: Parser Bool
sign =
  anyWord8 >>= \case
    43 -> return True
    45 -> return False
    _ -> empty

decimalOfLength :: (Integral a) => Int -> Parser a
decimalOfLength length =
  do
    bytes <- take length
    if C.all A.word8IsAsciiDigit bytes
      then return (A.decimalFromBytes bytes)
      else fail "Not all chars are valid decimals"

picoWithBasisOfLength :: Int -> Parser Pico
picoWithBasisOfLength basisLength =
  MkFixed <$> ((+) <$> beforePoint <*> ((word8 46 *> afterPoint) <|> pure 0))
  where
    beforePoint =
      (* (10 ^ 12)) <$> decimalOfLength basisLength
    afterPoint =
      fmap (updater . C.take 12) (takeWhile1 A.word8IsAsciiDigit)
      where
        updater bytes =
          let afterPoint =
                A.decimalFromBytes bytes
              afterPointLength =
                C.length bytes
              paddedAfterPoint =
                if afterPointLength < 12
                  then afterPoint * (10 ^ (12 - afterPointLength))
                  else afterPoint
           in paddedAfterPoint

{-# INLINE hour #-}
hour :: Parser Int
hour =
  validated B.hour (decimalOfLength 2) <?> "hour"

{-# INLINE minute #-}
minute :: Parser Int
minute =
  validated B.minute (decimalOfLength 2) <?> "minute"

{-# INLINE second #-}
second :: Parser Pico
second =
  validated B.second (picoWithBasisOfLength 2) <?> "second"

-- |
-- >>> parseOnly timeOfDayInISO8601 "05:03:58"
-- Right 05:03:58
--
-- >>> parseOnly timeOfDayInISO8601 "05:03:58.02"
-- Right 05:03:58.02
--
-- >>> parseOnly timeOfDayInISO8601 "05:03:58.020"
-- Right 05:03:58.02
--
-- Checks the elements to be within a proper range:
--
-- >>> parseOnly timeOfDayInISO8601 "24:00:00"
-- Left "timeOfDayInISO8601 > hour: Failed reading: Validator \"hour\" failed on the following input: 24"
--
-- >>> parseOnly timeOfDayInISO8601 "00:00:60"
-- Left "timeOfDayInISO8601 > second: Failed reading: Validator \"second\" failed on the following input: 60.000000000000"
--
-- Checks the elements to be of proper length:
--
-- >>> parseOnly timeOfDayInISO8601 "1:00:00"
-- Left "timeOfDayInISO8601 > hour: Failed reading: Not all chars are valid decimals"
--
-- >>> parseOnly timeOfDayInISO8601 "01:1:00"
-- Left "timeOfDayInISO8601 > minute: Failed reading: Not all chars are valid decimals"
{-# INLINE timeOfDayInISO8601 #-}
timeOfDayInISO8601 :: Parser TimeOfDay
timeOfDayInISO8601 =
  unnamedParser <?> "timeOfDayInISO8601"
  where
    unnamedParser =
      A.timeOfDay
        <$> (hour <* word8 58)
        <*> (minute <* word8 58)
        <*> (second)

-- |
-- >>> parseOnly dayInISO8601 "2017-02-01"
-- Right 2017-02-01
--
-- Checks the elements to be in proper range:
--
-- >>> parseOnly dayInISO8601 "2017-13-01"
-- Left "dayInISO8601: Failed reading: Invalid combination of year month and day: (2017,13,1)"
--
-- That is accounting for leap year:
--
-- >>> parseOnly dayInISO8601 "2017-02-29"
-- Left "dayInISO8601: Failed reading: Invalid combination of year month and day: (2017,2,29)"
--
-- >>> parseOnly dayInISO8601 "2016-02-29"
-- Right 2016-02-29
{-# INLINE dayInISO8601 #-}
dayInISO8601 :: Parser Day
dayInISO8601 =
  unnamedParser <?> "dayInISO8601"
  where
    unnamedParser =
      do
        year <- decimalOfLength 4
        word8 45
        month <- decimalOfLength 2
        word8 45
        day <- decimalOfLength 2
        case fromGregorianValid year month day of
          Just day -> return day
          Nothing -> fail (error year month day)
      where
        error year month day =
          showString "Invalid combination of year month and day: "
            $ show (year, month, day)

-- |
-- >>> parseOnly yearAndMonthInISO8601 "2016-02"
-- Right (2016,2)
yearAndMonthInISO8601 :: Parser (Word, Word)
yearAndMonthInISO8601 =
  unnamedParser <?> "yearAndMonthInISO8601"
  where
    unnamedParser =
      do
        year <- decimalOfLength 4
        word8 45
        month <- decimalOfLength 2
        return (year, month)

-- |
-- >>> parseOnly timeZoneInISO8601 "+01:00"
-- Right +0100
--
-- >>> parseOnly timeZoneInISO8601 "+0100"
-- Right +0100
--
-- >>> parseOnly timeZoneInISO8601 "-0100"
-- Right -0100
--
-- >>> parseOnly timeZoneInISO8601 "Z"
-- Right UTC
timeZoneInISO8601 :: Parser TimeZone
timeZoneInISO8601 =
  unnamedParser <?> "timeZoneInISO8601"
  where
    unnamedParser =
      z <|> offset
      where
        z =
          word8 90 $> utc
        offset =
          A.timeZone <$> sign <*> decimalOfLength 2 <*> (word8 58 *> decimalOfLength 2 <|> decimalOfLength 2 <|> pure 0)

-- |
-- >>> parseOnly utcTimeInISO8601 "2017-02-01T05:03:58+01:00"
-- Right 2017-02-01 04:03:58 UTC
utcTimeInISO8601 :: Parser UTCTime
utcTimeInISO8601 =
  unnamedParser <?> "utcTimeInISO8601"
  where
    unnamedParser =
      do
        day <- dayInISO8601
        word8 84
        time <- timeOfDayInISO8601
        zone <- timeZoneInISO8601
        return (A.utcTimeFromDayAndTimeOfDay day time zone)

-- |
-- No suffix implies the "seconds" unit:
--
-- >>> parseOnly diffTime "10"
-- Right 10s
--
-- Various units (seconds, minutes, hours, days):
--
-- >>> parseOnly diffTime "10s"
-- Right 10s
--
-- >>> parseOnly diffTime "10m"
-- Right 600s
--
-- >>> parseOnly diffTime "10h"
-- Right 36000s
--
-- >>> parseOnly diffTime "10d"
-- Right 864000s
--
-- Metric prefixes to seconds (down to Pico):
--
-- >>> parseOnly diffTime "10ms"
-- Right 0.01s
--
-- Notice that \"μs\" is not supported, because it's not ASCII.
--
-- >>> parseOnly diffTime "10us"
-- Right 0.00001s
--
-- >>> parseOnly diffTime "10ns"
-- Right 0.00000001s
--
-- >>> parseOnly diffTime "10ps"
-- Right 0.00000000001s
--
-- Negative values:
--
-- >>> parseOnly diffTime "-1s"
-- Right -1s
--
-- Unsupported units:
--
-- >>> parseOnly diffTime "1k"
-- Left "diffTime: Failed reading: Unsupported unit: \"k\""
diffTime :: Parser DiffTime
diffTime =
  unnamedParser <?> "diffTime"
  where
    unnamedParser =
      do
        amount <- D.scientific
        factor <- timeUnitFactor
        return (factor (realToFrac amount))

-- |
-- No suffix implies the "seconds" unit:
--
-- >>> parseOnly nominalDiffTime "10"
-- Right 10s
--
-- Various units (seconds, minutes, hours, days):
--
-- >>> parseOnly nominalDiffTime "10s"
-- Right 10s
--
-- >>> parseOnly nominalDiffTime "10m"
-- Right 600s
--
-- >>> parseOnly nominalDiffTime "10h"
-- Right 36000s
--
-- >>> parseOnly nominalDiffTime "10d"
-- Right 864000s
--
-- Metric prefixes to seconds (down to Pico):
--
-- >>> parseOnly nominalDiffTime "10ms"
-- Right 0.01s
--
-- Notice that \"μs\" is not supported, because it's not ASCII.
--
-- >>> parseOnly nominalDiffTime "10us"
-- Right 0.00001s
--
-- >>> parseOnly nominalDiffTime "10ns"
-- Right 0.00000001s
--
-- >>> parseOnly nominalDiffTime "10ps"
-- Right 0.00000000001s
--
-- Negative values:
--
-- >>> parseOnly nominalDiffTime "-1s"
-- Right -1s
--
-- Unsupported units:
--
-- >>> parseOnly nominalDiffTime "1k"
-- Left "nominalDiffTime: Failed reading: Unsupported unit: \"k\""
nominalDiffTime :: Parser NominalDiffTime
nominalDiffTime =
  unnamedParser <?> "nominalDiffTime"
  where
    unnamedParser =
      do
        amount <- D.scientific
        factor <- timeUnitFactor
        return (factor (realToFrac amount))

timeUnitFactor :: (Fractional a) => Parser (a -> a)
timeUnitFactor =
  takeWhile A.word8IsAsciiAlpha >>= \case
    "" -> return id
    "s" -> return id
    "ms" -> return (/ 1000)
    "μs" -> return (/ 1000000)
    "us" -> return (/ 1000000)
    "ns" -> return (/ 1000000000)
    "ps" -> return (/ 1000000000000)
    "m" -> return (* 60)
    "h" -> return (* 3600)
    "d" -> return (* 86400)
    unit -> fail ("Unsupported unit: " <> show unit)
