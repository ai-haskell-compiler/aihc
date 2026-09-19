module BlackholeChecks (blackholeChecks) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ArithException (Overflow), catch, evaluate, throwIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Each thunk remains active until its gate opens.
delayed :: MVar () -> MVar () -> IORef Int -> Bool -> Int -> Int
delayed started gate count fails value = unsafePerformIO $ do
  modifyIORef count (+ 1)
  putMVar started ()
  takeMVar gate
  if fails then throwIO Overflow else pure value
{-# NOINLINE delayed #-}

observe :: Int -> IO Int
observe value = catch (evaluate value) (\Overflow -> pure (-1))

-- The temporary constructors require collections while the three thunks remain active.
churn :: Int -> IO ()
churn remaining =
  if remaining == 0
    then pure ()
    else do
      evaluate (Just remaining)
      churn (remaining - 1)

checkMiddle :: Bool -> IO Bool
checkMiddle fails = do
  firstStarted <- newEmptyMVar
  middleStarted <- newEmptyMVar
  lastStarted <- newEmptyMVar
  firstGate <- newEmptyMVar
  middleGate <- newEmptyMVar
  lastGate <- newEmptyMVar
  firstDone <- newEmptyMVar
  middleDone <- newEmptyMVar
  lastDone <- newEmptyMVar
  firstCount <- newIORef 0
  middleCount <- newIORef 0
  lastCount <- newIORef 0
  let first = delayed firstStarted firstGate firstCount False 11
      middle = delayed middleStarted middleGate middleCount fails 22
      lastValue = delayed lastStarted lastGate lastCount False 33
  forkIO (observe first >>= putMVar firstDone)
  takeMVar firstStarted
  forkIO (observe middle >>= putMVar middleDone)
  takeMVar middleStarted
  forkIO (observe lastValue >>= putMVar lastDone)
  takeMVar lastStarted
  waiting <- newEmptyMVar
  waiterOne <- newEmptyMVar
  waiterTwo <- newEmptyMVar
  forkIO (putMVar waiting () >> observe middle >>= putMVar waiterOne)
  takeMVar waiting
  forkIO (putMVar waiting () >> observe middle >>= putMVar waiterTwo)
  takeMVar waiting
  churn 2000
  -- Complete the middle thunk before the oldest and newest thunks.
  putMVar middleGate ()
  middleResult <- takeMVar middleDone
  one <- takeMVar waiterOne
  two <- takeMVar waiterTwo
  -- An exception restores the thunk so that a later evaluation can retry it.
  retried <- if fails
    then do
      forkIO (observe middle >>= putMVar middleDone)
      takeMVar middleStarted
      putMVar middleGate ()
      takeMVar middleDone
    else observe middle
  putMVar firstGate ()
  firstResult <- takeMVar firstDone
  putMVar lastGate ()
  lastResult <- takeMVar lastDone
  firstRuns <- readIORef firstCount
  middleRuns <- readIORef middleCount
  lastRuns <- readIORef lastCount
  let expected = if fails then -1 else 22
      expectedRuns = if fails then 2 else 1
  pure (firstResult == 11 && lastResult == 33 && firstRuns == 1 && lastRuns == 1
    && middleResult == expected && one == expected && two == expected
    && retried == expected && middleRuns == expectedRuns)

blackholeChecks :: IO Bool
blackholeChecks = do
  success <- checkMiddle False
  exception <- checkMiddle True
  pure (success && exception)
