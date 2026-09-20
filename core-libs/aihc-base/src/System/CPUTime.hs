{-# LANGUAGE CApiFFI #-}

-- | Process CPU time in picoseconds.
module System.CPUTime (getCPUTime, cpuTimePrecision) where

import Foreign.C.Types (CClock (..))
import GHC.Internal.IO.Types (ioError, unsupportedOperation)
import System.CPUTime.Clock (clock)
import System.IO.Error (ioeSetLocation)
import Prelude (Bounded (..), IO, Integer, Integral (..), return, (*), (==))

-- | Return the CPU time that this process has used.
getCPUTime :: IO Integer
getCPUTime = do
  ticks <- clock
  if ticks == maxBound
    then ioError (ioeSetLocation unsupportedOperation "getCPUTime")
    else return (toInteger ticks * picosecondsPerSecond `quot` toInteger clocksPerSecond)

-- | The duration of one clock tick in picoseconds.
cpuTimePrecision :: Integer
cpuTimePrecision = picosecondsPerSecond `quot` toInteger clocksPerSecond

picosecondsPerSecond :: Integer
picosecondsPerSecond = 1000000000000

foreign import capi unsafe "time.h value CLOCKS_PER_SEC"
  clocksPerSecond :: CClock
