{-| This module extend vformat to 'FormatTime' datatypes

The specs of 'FormatTime' is same as the second argument of
"Data.Time.Format.formatTime".

=== Examples

>>> utc <- getCurrentTime
>>> zoned <- getZonedTime
>>> let locale = utcToLocalTime (zonedTimeZone zoned) utc
>>> format "{} / {} / {}" utc zoned locale
"2020-01-21 05:54:41.137832 / 2020-01-21 13:54:45.310354 CST / 2020-01-21 13:54:41.137832"
>>> format "{:%Y-%m-%dT%H:%M:%S}" utc
"2020-01-21T05:54:41"
-}
module Text.Format.Time
  ( formatTimeLocale
  , formatTime
    ) where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format    hiding (formatTime)
import qualified Data.Time.Format    as T
import           Data.Time.LocalTime
import           Text.Format


-- | Default specs is @%F@
instance FormatArg Day where
  formatArg = defaultSpecs "%F" formatTime

-- | Default specs is @%F %T%Q@
instance FormatArg UTCTime where
  formatArg = formatTime

-- | Default specs is @%F %T%Q@
instance FormatArg UniversalTime where
  formatArg = formatTime

-- | Default specs is @%Z@
instance FormatArg TimeZone where
  formatArg = defaultSpecs "%Z" formatTime

-- | Default specs is @%T@
instance FormatArg TimeOfDay where
  formatArg = defaultSpecs "%T" formatTime

-- | Default specs is @%F %T%Q@
instance FormatArg LocalTime where
  formatArg = formatTime

-- | Default specs is @%F %T%Q %Z@
instance FormatArg ZonedTime where
  formatArg = defaultSpecs "%F %T%Q %Z" formatTime


{-| Formatter for 'FormatTime' values using a specific 'TimeLocale'. -}
formatTimeLocale :: FormatTime t => TimeLocale -> t -> Formatter
formatTimeLocale locale = defaultSpecs "%F %T%Q" $ \x k fmt ->
  formatString (T.formatTime locale (fmtSpecs fmt) x) k (fmt{fmtSpecs=""})


{-| Formatter for 'FormatTime' values using the 'defaultTimeLocale'. -}
formatTime :: FormatTime t => t -> Formatter
formatTime = formatTimeLocale defaultTimeLocale
