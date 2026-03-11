{-# LANGUAGE CPP #-}

module Model where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Time
import Data.Time.Clock.POSIX
import System.Posix.Types

#if !MIN_VERSION_time(1,5,0)
import System.Locale
#endif

epochTimeToUtcTime :: EpochTime -> UTCTime
epochTimeToUtcTime = posixSecondsToUTCTime . realToFrac

utcToDate :: UTCTime -> ByteString
utcToDate = BS8.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
