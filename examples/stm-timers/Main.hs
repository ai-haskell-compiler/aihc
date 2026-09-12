module Main where

import GHC.Conc

wait :: TVar Bool -> STM ()
wait timer = do
  ready <- readTVar timer
  if ready then pure () else retry

main :: IO ()
main = do
  immediate <- registerDelay 0
  negative <- registerDelay (-1)
  a <- readTVarIO immediate
  b <- readTVarIO negative
  value <- newTVarIO (7 :: Int)
  first <- registerDelay 1000
  second <- registerDelay 20000
  atomically $
    (writeTVar value 99 >> retry)
      `orElse` (wait first >> wait second)
  c <- readTVarIO value
  d <- readTVarIO first
  e <- readTVarIO second
  third <- registerDelay 1000
  atomically (wait third)
  putStrLn (if a && b && c == 7 && d && e then "STM timers: ok" else "STM timers: failed")
