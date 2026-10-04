{-# LANGUAGE ForeignFunctionInterface #-}

-- | A monotonic clock. Its start point is not specified, so only the
-- difference between two readings has a meaning.
module GHC.Clock
  ( getMonotonicTime,
    getMonotonicTimeNSec,
  )
where

import GHC.Word (Word64)
import Prelude

-- | The time of the monotonic clock in seconds.
getMonotonicTime :: IO Double
getMonotonicTime = do
  nanoseconds <- getMonotonicTimeNSec
  return (fromIntegral nanoseconds / 1.0e9)

-- | The time of the monotonic clock in nanoseconds.
getMonotonicTimeNSec :: IO Word64
getMonotonicTimeNSec = monotonicNanoseconds

foreign import ccall unsafe "aihc_clock_monotonic_ns"
  monotonicNanoseconds :: IO Word64
