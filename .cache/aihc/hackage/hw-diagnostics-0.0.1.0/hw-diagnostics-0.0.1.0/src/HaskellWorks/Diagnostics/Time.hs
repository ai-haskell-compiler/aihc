{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Diagnostics.Time where

import           System.CPUTime

measure :: a -> IO a
measure a = measureIO (return a)

measureIO :: IO a -> IO a
measureIO a = do
  start <- getCPUTime
  !b <- a
  end   <- getCPUTime
  putStrLn $ show ((end - start) `div` 1000000000) ++ " ms"
  return b
