-----------------------------------------------------------------------------
-- |
-- Module      :  System.LibCPUID.TSC
-- Copyright   :  (c) Daniel Taskoff, 2020
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com
-- Stability   :  experimental
--
-- Calculate the CPU clock, by using a time-stamp counter.
--
-- Usage:
--
-- > do
-- >   tscMark <- initialise
-- >   mark tscMark
-- >   foo
-- >   unmark tscMark
-- >   clock <- clockBy tscMark
-----------------------------------------------------------------------------

module System.LibCPUID.TSC
  (
  -- * 'TSCMark'
    TSCMark, initialise
  , mark, unmark, clockBy
  ) where

import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr)


#include "libcpuid.h"

newtype TSCMark = TSCMark (ForeignPtr ())

-- | Initialise a 'TSCMark'.
initialise :: IO TSCMark
initialise = TSCMark <$> mallocForeignPtrBytes #{size struct cpu_mark_t}

-- | Mark a time interval for calculating the CPU clock frequency.
mark :: TSCMark -> IO ()
mark (TSCMark ptr) = withForeignPtr ptr c_cpu_tsc_mark

foreign import ccall "cpu_tsc_mark"
  c_cpu_tsc_mark :: Ptr cpu_mark_t -> IO ()

-- | Unmark a time interval for calculating the CPU clock frequency.
unmark :: TSCMark -> IO ()
unmark (TSCMark ptr) = withForeignPtr ptr c_cpu_tsc_unmark

foreign import ccall "cpu_tsc_unmark"
  c_cpu_tsc_unmark :: Ptr cpu_mark_t -> IO ()

-- | Calculate the CPU clock frequency in MHz, for an interval, marked with 'mark', and 'unmark'.
-- If the interval is insufficient, the result will be -1.
clockBy :: TSCMark -> IO Int
clockBy (TSCMark ptr) = fromIntegral <$> withForeignPtr ptr c_cpu_clock_by_mark

foreign import ccall "cpu_clock_by_mark"
  c_cpu_clock_by_mark :: Ptr cpu_mark_t -> IO CInt
