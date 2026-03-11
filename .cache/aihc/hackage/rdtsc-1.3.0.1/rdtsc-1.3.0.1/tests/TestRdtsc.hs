module Main where

import System.CPUTime.Rdtsc

main = do
  t1 <- rdtsc
  t2 <- rdtsc
  putStrLn ("Cost of rdtsc (ffi call):    " ++ show (t2 - t1))
