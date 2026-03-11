-- |
-- Stability   :  Ultra-Violence
-- Portability :  I'm too young to die
-- XEP-0082: XMPP Date and Time Profiles

module Network.Xmpp.Extras.DateTime where

import Data.Time

toDateTime :: UTCTime -> String
toDateTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"

fromDateTime :: String -> Maybe UTCTime
fromDateTime = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
