module TimerWheel.Internal.Nanoseconds
  ( Nanoseconds (..),
    fromSeconds,
    fromNonNegativeSeconds,
    div,
    unsafeMinus,
    sleep,
  )
where

import Control.Concurrent (threadDelay)
import Data.Fixed (Fixed (..))
import TimerWheel.Internal.Prelude hiding (div)
import qualified Prelude

-- Some positive number of nanoseconds
newtype Nanoseconds = Nanoseconds {unNanoseconds :: Word64}
  deriving stock (Eq, Ord)

-- | Convert a number of seconds into a number of nanoseconds.
--
-- Negative values are converted to 0.
fromSeconds :: Seconds -> Nanoseconds
fromSeconds =
  fromNonNegativeSeconds . max 0

-- | Like 'fromNonNegativeSeconds', but with an unchecked precondition: the given seconds is non-negative.
--
-- What you get for your troubles: one puny fewer int comparisons.
fromNonNegativeSeconds :: Seconds -> Nanoseconds
fromNonNegativeSeconds seconds =
  Nanoseconds (coerce @(Integer -> Word64) fromIntegral seconds)

div :: Nanoseconds -> Nanoseconds -> Nanoseconds
div =
  coerce (Prelude.div @Word64)

-- `unsafeMinus n m` subtracts `m` from `n`, but does something wild if `m` is bigger than `n`
unsafeMinus :: Nanoseconds -> Nanoseconds -> Nanoseconds
unsafeMinus =
  coerce ((-) @Word64)

sleep :: Nanoseconds -> IO ()
sleep (Nanoseconds nanos) =
  threadDelay (fromIntegral (nanos `Prelude.div` 1000))
