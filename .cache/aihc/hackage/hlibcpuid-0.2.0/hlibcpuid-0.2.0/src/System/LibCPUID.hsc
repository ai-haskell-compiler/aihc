-----------------------------------------------------------------------------
-- |
-- Module      :  System.LibCPUID
-- Copyright   :  (c) Daniel Taskoff, 2020
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com
-- Stability   :  experimental
--
-- Bindings to https://github.com/anrieff/libcpuid.
--
-- Currently implemented:
--
-- * getting CPU vendor and brand strings, TSC availability, number of physical, and logical cores
-- * measuring CPU clock frequency using a time-stamp counter and time intervals
-- * getting CPU clock frequency as reported by the OS
-- * measuring CPU clock frequency using a busy-wait cycle
-----------------------------------------------------------------------------

module System.LibCPUID
  (
  -- * LibCPUID utilities
    getTotalLogicalCores
  , clockByOS
  , clockMeasure, ShouldQuadCheck(..)
  , clock
  -- * Reexports from "System.LibCPUID.CPUID"
  , module System.LibCPUID.CPUID
  -- * Reexports from "System.LibCPUID.TSC"
  , module System.LibCPUID.TSC
  ) where

import Foreign.C.Types (CInt(..))
import System.LibCPUID.CPUID (CPUID(..), cpuid)
import System.LibCPUID.TSC (TSCMark, initialise, mark, unmark, clockBy)


#include "libcpuid.h"

-- | Get the total number of logical cores (even if CPUID is not present).
-- If CPUID is present, the following is true:
--
-- * @'getTotalLogicalCores' = 'totalLogicalCores' '<$>' 'cpuid'@
getTotalLogicalCores :: IO Int
getTotalLogicalCores = fromIntegral <$> c_cpuid_get_total_cpus

foreign import ccall "cpuid_get_total_cpus"
  c_cpuid_get_total_cpus :: IO CInt

-- | Get the CPU clock frequency in MHz, as reported by the OS (which may differ from the true clock).
-- If the OS is not supported, the result will be -1.
clockByOS :: IO Int
clockByOS = fromIntegral <$> c_cpu_clock_by_os

foreign import ccall "cpu_clock_by_os"
  c_cpu_clock_by_os :: IO CInt

-- | Get the CPU clock frequency in MHz, after performing a busy-wait cycle for the given time in ms.
-- If RDTSC is not supported, the result will be -1.
clockMeasure :: Int -> ShouldQuadCheck -> IO Int
clockMeasure time (ShouldQuadCheck shouldQuadCheck) =
  fromIntegral <$> c_cpu_clock_measure (fromIntegral time) if shouldQuadCheck then 1 else 0

-- | Should 'clockMeasure' do a more thorough measurement (quadruple checking).
newtype ShouldQuadCheck = ShouldQuadCheck Bool

foreign import ccall "cpu_clock_measure"
  c_cpu_clock_measure :: CInt -> CInt -> IO CInt

-- | Get the CPU clock frequency in MHz, by trying all available methods.
-- If all of them fail, the result will be -1.
clock :: IO Int
clock = fromIntegral <$> c_cpu_clock

foreign import ccall "cpu_clock"
  c_cpu_clock :: IO CInt
