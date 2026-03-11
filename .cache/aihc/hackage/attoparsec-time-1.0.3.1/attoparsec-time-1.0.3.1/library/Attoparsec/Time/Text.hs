{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Attoparsec.Time.Text
  ( timeOfDayInISO8601,
    timeOfDayInDashes,
    dayInISO8601,
    yearAndMonthInISO8601,
    timeZoneInISO8601,
    utcTimeInISO8601,
    utcTimeInDashes,
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
import Data.Attoparsec.Text
import qualified Data.Text as C

validated :: (Show a) => B.Validator a -> Parser a -> Parser a
validated validator parser =
  parser >>= \x -> B.run validator (pure x) fail x

sign :: Parser Bool
sign =
  anyChar >>= \case
    '+' -> return True
    '-' -> return False
    _ -> empty

decimalChar :: Parser Int
decimalChar =
  satisfyWith ((subtract 48) . ord) ((&&) <$> (>= 0) <*> (<= 9))

decimalOfLength :: (Num a) => Int -> Parser a
decimalOfLength length =
  foldl' (\a b -> a * 10 + b) 0
    <$> replicateM length (fmap fromIntegral decimalChar)
    <|> fail "Invalid decimal length"

shortMonth :: Parser Int
shortMonth =
  liftM C.toLower (take 3) >>= \case
    "jan" -> return 1
    "feb" -> return 2
    "mar" -> return 3
    "apr" -> return 4
    "may" -> return 5
    "jun" -> return 6
    "jul" -> return 7
    "aug" -> return 8
    "sep" -> return 9
    "oct" -> return 10
    "nov" -> return 11
    "dec" -> return 12
    _ -> empty

picoWithBasisOfLength :: Int -> Parser Pico
picoWithBasisOfLength basisLength =
  (\a b -> MkFixed (foldl' (\a b -> a * 10 + fromIntegral b) 0 (a ++ b))) <$> beforePoint <*> afterPoint
  where
    resolution =
      12
    beforePoint =
      replicateM basisLength decimalChar
    afterPoint =
      padListFromRight 0 [] resolution <$> ((char '.' *> many1 decimalChar) <|> pure [])
      where
        padListFromRight padding accumulator length list =
          case length of
            0 -> reverse accumulator
            _ -> case list of
              head : tail -> padListFromRight padding (head : accumulator) (pred length) tail
              _ -> reverse accumulator ++ replicate length padding

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
-- Left "timeOfDayInISO8601 > hour: Failed reading: Invalid decimal length"
--
-- >>> parseOnly timeOfDayInISO8601 "01:1:00"
-- Left "timeOfDayInISO8601 > minute: Failed reading: Invalid decimal length"
{-# INLINE timeOfDayInISO8601 #-}
timeOfDayInISO8601 :: Parser TimeOfDay
timeOfDayInISO8601 =
  unnamedParser <?> "timeOfDayInISO8601"
  where
    unnamedParser =
      A.timeOfDay
        <$> (hour <* char ':')
        <*> (minute <* char ':')
        <*> (second)

{-# INLINE timeOfDayInDashes #-}
timeOfDayInDashes :: Parser TimeOfDay
timeOfDayInDashes =
  unnamedParser <?> "timeOfDayInDashes"
  where
    unnamedParser =
      A.timeOfDay
        <$> (hour <* char '-')
        <*> (minute <* char '-')
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
        char '-'
        month <- decimalOfLength 2
        char '-'
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
        char '-'
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
          char 'Z' $> utc
        offset =
          A.timeZone <$> sign <*> decimalOfLength 2 <*> (char ':' *> decimalOfLength 2 <|> decimalOfLength 2 <|> pure 0)

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
        char 'T'
        time <- timeOfDayInISO8601
        zone <- timeZoneInISO8601
        return (A.utcTimeFromDayAndTimeOfDay day time zone)

-- |
-- >>> parseOnly utcTimeInDashes "2017-02-01-05-03-58"
-- Right 2017-02-01 05:03:58 UTC
utcTimeInDashes :: Parser UTCTime
utcTimeInDashes =
  unnamedParser <?> "utcTimeInDashes"
  where
    unnamedParser =
      do
        day <- dayInISO8601
        char '-'
        time <- timeOfDayInDashes
        return (localTimeToUTC utc (LocalTime day time))

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
-- >>> parseOnly diffTime "10μs"
-- Right 0.00001s
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
        amount <- scientific
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
-- >>> parseOnly nominalDiffTime "10μs"
-- Right 0.00001s
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
        amount <- scientific
        factor <- timeUnitFactor
        return (factor (realToFrac amount))

timeUnitFactor :: (Fractional a) => Parser (a -> a)
timeUnitFactor =
  takeWhile isAlpha >>= \case
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
