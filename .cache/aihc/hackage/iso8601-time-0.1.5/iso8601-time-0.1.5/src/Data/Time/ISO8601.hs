{-# LANGUAGE CPP #-}
module Data.Time.ISO8601
  ( formatISO8601
  , formatISO8601Millis
  , formatISO8601Micros
  , formatISO8601Nanos
  , formatISO8601Picos
  , formatISO8601Javascript
  , parseISO8601
  ) where


import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
#else
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)
#endif
import Control.Applicative ((<|>))


-- | Formats a time in ISO 8601, with up to 12 second decimals.
--
-- This is the `formatTime` format @%FT%T%Q@ == @%%Y-%m-%dT%%H:%M:%S%Q@.
formatISO8601 :: UTCTime -> String
formatISO8601 t = formatTime defaultTimeLocale "%FT%T%QZ" t


-- | Pads an ISO 8601 date with trailing zeros, but lacking the trailing Z.
--
-- This is needed because `formatTime` with "%Q" does not create trailing zeros.
formatPadded :: UTCTime -> String
formatPadded t
  | length str == 19 = str ++ ".000000000000"
  | otherwise        = str ++ "000000000000"
  where
    str = formatTime defaultTimeLocale "%FT%T%Q" t


-- | Formats a time in ISO 8601 with up to millisecond precision and trailing zeros.
-- The format is precisely:
--
-- >YYYY-MM-DDTHH:mm:ss.sssZ
formatISO8601Millis :: UTCTime -> String
formatISO8601Millis t = take 23 (formatPadded t) ++ "Z"


-- | Formats a time in ISO 8601 with up to microsecond precision and trailing zeros.
-- The format is precisely:
--
-- >YYYY-MM-DDTHH:mm:ss.ssssssZ
formatISO8601Micros :: UTCTime -> String
formatISO8601Micros t = take 26 (formatPadded t) ++ "Z"


-- | Formats a time in ISO 8601 with up to nanosecond precision and trailing zeros.
-- The format is precisely:
--
-- >YYYY-MM-DDTHH:mm:ss.sssssssssZ
formatISO8601Nanos :: UTCTime -> String
formatISO8601Nanos t = take 29 (formatPadded t) ++ "Z"

-- | Formats a time in ISO 8601 with up to picosecond precision and trailing zeros.
-- The format is precisely:
--
-- >YYYY-MM-DDTHH:mm:ss.ssssssssssssZ
formatISO8601Picos :: UTCTime -> String
formatISO8601Picos t = take 32 (formatPadded t) ++ "Z"


-- | Formats a time like JavaScript's @new Date().toISOString()@
-- as specified by Mozilla: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString>
--
-- This is an alias for `formatISO8601Millis`.
formatISO8601Javascript :: UTCTime -> String
formatISO8601Javascript = formatISO8601Millis


-- | Parses an ISO 8601 string.
--
-- Leading and trailing whitespace is accepted. See `parseTimeM` from the
-- `time` package for more details.
parseISO8601 :: String -> Maybe UTCTime
#if MIN_VERSION_time(1,5,0)
parseISO8601 t = parseTimeM True defaultTimeLocale "%FT%T%QZ" t <|>
                 parseTimeM True defaultTimeLocale "%FT%T%Q%z" t
#else
parseISO8601 t = parseTime defaultTimeLocale "%FT%T%QZ" t <|>
                 parseTime defaultTimeLocale "%FT%T%Q%z" t
#endif
