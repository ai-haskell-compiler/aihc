{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Data.Maybe
import System.Environment
import System.Exit
import System.Process
import System.IO

import System.FileLock

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ["shared", read -> duration]
      -> holdLock "shared" Shared duration
    ["exclusive", read -> duration]
      -> holdLock "exclusive" Exclusive duration
    ["try"]
      -> tryTakingLock
    ["tryshared", read -> duration]
      -> tryHoldLock "shared" Shared duration
    ["tryexclusive", read -> duration]
      -> tryHoldLock "exclusive" Exclusive duration
    _ -> do
      withFile "lock.log" WriteMode $ \h ->
        void $ mapConcurrently id
          [ callSelf h ["shared", "5"]
          , callSelf h ["shared", "2"]
          , sleep 1 >> callSelf h ["exclusive", "3"]
          , sleep 3 >> callSelf h ["try"]
          , sleep 4 >> callSelf h ["shared", "1"]
          , sleep 6 >> callSelf h ["shared", "1"]
          , sleep 10 >> callSelf h ["try"]
          ]
      sleep 11
      log <- readFile "lock.log"
      expected <- readFile "tests/lock.log.expected"
      when (log /= expected) $ do
        putStrLn "log mismatch!"
        putStrLn "log:"
        putStrLn log
        putStrLn "expected:"
        putStrLn expected
        exitFailure

callSelf :: Handle -> [String] -> IO ()
callSelf out args = do
  self <- getExecutablePath
  (_hin, _hout, _herr, ph) <- createProcess_ "callSelf"
    (proc self args) { std_out = UseHandle out }
  ExitSuccess <- waitForProcess ph
  return ()

sleep :: Int -> IO ()
sleep = threadDelay . (*1000000)

holdLock :: String -> SharedExclusive -> Int -> IO ()
holdLock ty sex duration = do
  withFileLock lockfile sex $ \_ -> do
    putStrLn $ "took " ++ desc
    sleep duration
    putStrLn $ "releasing " ++ desc
  where
    desc = ty ++ " lock"

tryTakingLock :: IO ()
tryTakingLock = do
  ml <- tryLockFile lockfile Exclusive
  case ml of
    Nothing -> putStrLn "lock not available"
    Just l -> do
      putStrLn "lock was available"
      unlockFile l

tryHoldLock :: String -> SharedExclusive -> Int -> IO ()
tryHoldLock ty sex duration = do
  res <- withTryFileLock lockfile sex $ \_ -> do
    putStrLn $ "took " ++ desc
    sleep duration
    putStrLn $ "released " ++ desc
  when (isNothing res) $ putStrLn "lock not available"
  where
    desc = ty ++ " lock"

lockfile :: String
lockfile = "lock"
