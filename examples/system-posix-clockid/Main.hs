{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- A clock id is the C type clockid_t. That type is an integer on most
-- platforms and a pointer under wasi-libc, so the C wrapper of a capi
-- import has to spell it.
module Main where

import System.Posix.Types (CClockId)

foreign import capi "time.h value CLOCK_REALTIME" clockRealtime :: CClockId

foreign import capi "time.h value CLOCK_MONOTONIC" clockMonotonic :: CClockId

main :: IO ()
main = do
  putStrLn (if clockRealtime /= clockMonotonic then "clocks differ" else "clocks equal")
  putStrLn (if clockRealtime == clockRealtime then "clock is itself" else "clock changes")
