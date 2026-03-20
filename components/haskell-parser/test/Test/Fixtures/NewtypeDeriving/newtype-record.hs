newtype TimeSince = TimeSince
  { sinceRef :: IORef UTCTime
  }
