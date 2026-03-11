module Data.Time.Since (TimeSince, newTimeSince, timeSince) where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)


newtype TimeSince = TimeSince
  { sinceRef :: IORef UTCTime
  }


newTimeSince :: IO TimeSince
newTimeSince = do
  n <- getCurrentTime
  TimeSince <$> newIORef n


timeSince :: TimeSince -> IO NominalDiffTime
timeSince TimeSince{sinceRef = sinceRef} = do
  n <- readIORef sinceRef
  m <- getCurrentTime
  writeIORef sinceRef m
  pure (diffUTCTime m n)
