module TimerWheel.Internal.Timestamp
  ( Timestamp (..),
    epoch,
    intoEpoch,
    unsafeMinus,
    plus,
    now,
  )
where

import GHC.Clock (getMonotonicTimeNSec)
import TimerWheel.Internal.Nanoseconds (Nanoseconds (..))
import TimerWheel.Internal.Prelude
import qualified Prelude

-- Monotonic time, in nanoseconds
newtype Timestamp
  = Timestamp Nanoseconds
  deriving stock (Eq, Ord)

-- Which epoch does this correspond to, if they are measured in chunks of the given number of nanoseconds?
epoch :: Nanoseconds -> Timestamp -> Word64
epoch x y =
  coerce @_ @Word64 y `div` coerce x

intoEpoch :: Timestamp -> Nanoseconds -> Nanoseconds
intoEpoch =
  coerce (Prelude.rem @Word64)

unsafeMinus :: Timestamp -> Timestamp -> Nanoseconds
unsafeMinus =
  coerce ((-) @Word64)

plus :: Timestamp -> Nanoseconds -> Timestamp
plus =
  coerce ((+) @Word64)

now :: IO Timestamp
now =
  coerce getMonotonicTimeNSec
