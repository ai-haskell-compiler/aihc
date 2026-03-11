--------------------------------------------------------------------------
-- |
-- Module:     System.CPUTime.Rdtsc
-- Copyright:  (c) 2006-2007,2010 Martin Grabmueller
-- License:    GPL
-- 
-- Maintainer:  martin@grabmueller.de
-- Stability:   provisional
-- Portability: non-portable (requires IA-32  processor and GHC)
--
-- This module provides the function "rdtsc" for accessing the rdtsc
-- machine register on modern IA-32 processors.  This is a 64-bit
-- counter which counts the number of processor cycles since the
-- machine has been powered up.
--
-- As an example, you may use the following program to measure the overhead
-- of calling this function:
--
-- > module Main where
-- > import System.CPUTime.Rdtsc
-- >
-- > main = do
-- >   t1 <- rdtsc
-- >   t2 <- rdtsc
-- >   putStrLn ("Cost of rdtsc (ffi call):    " ++ show (t2 - t1))
--------------------------------------------------------------------------
module System.CPUTime.Rdtsc(rdtsc) where

import Foreign

-- | Return the number of clock cycles since the machine this function
-- is called on has been powered up.  Note that the value returned
-- may be inaccurate if the program is run on some hardware
-- virtualization layer.
--
-- Especially on multi-core processors and when using hibernating
-- operating systems, the values returned may be bogus.
--
-- For more information about the @rdtsc@ instruction, see 
-- <http://en.wikipedia.org/wiki/RDTSC>
foreign import ccall unsafe "rdtsc.h" rdtsc :: IO Word64
