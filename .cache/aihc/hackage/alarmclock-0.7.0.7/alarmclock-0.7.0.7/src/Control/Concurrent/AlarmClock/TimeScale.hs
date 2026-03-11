
{-| Abstraction that allows for a choice of timescale when setting alarms.
Exposed in a separate module so it can be faked for testing purposes, but
client applications should just import "Control.Concurrent.AlarmClock". -}

module Control.Concurrent.AlarmClock.TimeScale where

import           Data.Time    (UTCTime, diffUTCTime, getCurrentTime)
import           System.Clock (Clock (Monotonic), TimeSpec, diffTimeSpec,
                               getTime, toNanoSecs)

{-| Abstraction that allows for a choice between the UTC timescale and a
monotonic timescale, which differ in their handling of irregularities such as
clock adjustments and leap seconds.

Alarms set using the 'UTCTime' timescale wait for the system clock to pass the
given time before going off, and account for the clock being adjusted
backwards and for (positive) leap seconds while waiting. If the clock is set
forwards, or a negative leap second occurs, then the alarm may go off later
than expected by an amount that is roughly equal to the adjustment. It is
possible to correct for this by setting the alarm again after the adjustment
has occurred.

The 'Monotonic' timescale cannot be so adjusted, which may be more suitable for
some applications.

Note that the timeliness of the alarm going off is very much on a "best effort"
basis, and there are many environmental factors that could cause the alarm to
go off later than expected.

-}

class Eq t => TimeScale t where
  getAbsoluteTime   :: IO t
  microsecondsDiff  :: t -> t -> Integer
  earlierOf         :: t -> t -> t

instance TimeScale UTCTime where
  getAbsoluteTime        = getCurrentTime
  earlierOf              = min
  microsecondsDiff t1 t2 = ceiling $ (1000000 *) $ diffUTCTime t1 t2

{-| Representation of system monotonic clock. -}
newtype MonotonicTime = MonotonicTime TimeSpec deriving (Show, Eq, Ord)

instance TimeScale MonotonicTime where
  getAbsoluteTime = MonotonicTime <$> getTime Monotonic
  earlierOf       = min
  microsecondsDiff (MonotonicTime t1) (MonotonicTime t2)
    | t1 < t2 = -microsecondsDiff (MonotonicTime t2) (MonotonicTime t1)
    | otherwise = (`div` 1000) $ toNanoSecs $ diffTimeSpec t1 t2
