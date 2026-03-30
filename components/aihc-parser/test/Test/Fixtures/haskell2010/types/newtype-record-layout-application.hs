{- ORACLE_TEST
id: types-newtype-record-layout-application
category: types
expected: pass
-}
module TNewtypeRecordLayoutApplication where

import Data.IORef (IORef)
import Data.Time.Clock (UTCTime)

newtype TimeSince = TimeSince
  { sinceRef :: IORef UTCTime
  }
