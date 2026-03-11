{-# LANGUAGE OverloadedStrings #-}
module EasyLogger.Date
    ( FormattedTime
    , TimeFormat
    , newTimeCache
    , simpleTimeFormat
    , simpleTimeFormat'
    ) where

import           Control.AutoUpdate       (defaultUpdateSettings, mkAutoUpdate,
                                           updateAction)
import           Data.ByteString
import           Data.UnixTime            (formatUnixTime, fromEpochTime)
import           System.PosixCompat.Time  (epochTime)
import           System.PosixCompat.Types (EpochTime)


-- | Type aliaes for date format and formatted date.
type FormattedTime = ByteString
type TimeFormat = ByteString


----------------------------------------------------------------

-- | Get date using UnixTime.
getTime :: IO EpochTime
getTime = epochTime

-- | Format unix EpochTime date.
formatDate :: TimeFormat -> EpochTime -> IO FormattedTime
formatDate fmt = formatUnixTime fmt . fromEpochTime

----------------------------------------------------------------

-- |  Make 'IO' action which get cached formatted local time.
-- Use this to avoid the cost of frequently time formatting by caching an
-- auto updating formatted time, this cache update every 1 second.
-- more detail in "Control.AutoUpdate"
newTimeCache :: TimeFormat -> IO (IO FormattedTime)
newTimeCache fmt = mkAutoUpdate defaultUpdateSettings {updateAction = getTime >>= formatDate fmt}

-- | A simple time cache using format @"%d/%b/%Y:%T %z"@
simpleTimeFormat :: TimeFormat
simpleTimeFormat = "%d/%b/%Y:%T %z"

-- | A simple time cache using format @"%d-%b-%Y %T"@
simpleTimeFormat' :: TimeFormat
simpleTimeFormat' = "%d-%b-%Y %T"
