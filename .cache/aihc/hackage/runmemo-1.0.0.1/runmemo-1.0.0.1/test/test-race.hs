module Main where

import Data.MemoCombinators as Memo
import Data.RunMemo
import System.Exit
import Data.Time.Clock
import Text.Printf

fib :: Memoizable (Integer -> Integer)
fib self = go
  where go 0 = 1
        go 1 = 1
        go n = self (n-1) + self (n-2)

time :: IO () -> IO NominalDiffTime
time a = do
  start <- getCurrentTime
  a
  stop <- getCurrentTime
  return $ diffUTCTime stop start

main :: IO ()
main = do
  putStrLn "Evaluating with Memo.integral"
  mtime <- time $ runMemo Memo.integral fib 30 `seq` putStrLn "Evaluated"
  printf "Memo.integral took %s\n" (show mtime)
  putStrLn "Evaluating with noMemo"
  ntime <- time $ runMemo noMemo fib 30 `seq` putStrLn "Evaluated"
  printf "noMemo        took %s\n" (show ntime)
  if mtime < ntime
    then exitSuccess
    else exitFailure
