{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, isEmptyMVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (evaluate)
import GHC.Exts (Int (..), Int#, MutableByteArray#, RealWorld, fetchAddIntArray#, newByteArray#, readIntArray#, writeIntArray#, (-#), (>=#))
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafePerformIO)

-- Two threads on two capabilities force one value of unsafePerformIO. Its
-- action must run once: the second thread waits for the first one. See
-- "noDuplicate#" in docs/multi-threaded-rts-design.md.
--
-- The action allocates nothing and stores no pointer, so no collection and
-- no promotion pauses its thread. Only noDuplicate# claims the value.

data Counter = Counter (MutableByteArray# RealWorld)

{-# NOINLINE runs #-}
runs :: Counter
runs = unsafePerformIO $ IO $ \state -> case newByteArray# 8# state of
  (# next, array #) -> case writeIntArray# array 0# 0# next of
    done -> (# done, Counter array #)

count :: IO Int
count = case runs of
  Counter array -> IO $ \state -> case readIntArray# array 0# state of
    (# next, value #) -> (# next, I# value #)

{-# NOINLINE entering #-}
entering :: MVar ()
entering = unsafePerformIO newEmptyMVar

-- Wait until a variable is full, at most a number of times.
waitFull :: Int# -> IO ()
waitFull 0# = return ()
waitFull remaining = do
  empty <- isEmptyMVar entering
  if empty then waitFull (remaining -# 1#) else return ()

-- Wait until the action ran twice, at most a number of times.
waitTwice :: Int# -> IO ()
waitTwice 0# = return ()
waitTwice remaining = case runs of
  Counter array -> IO $ \state -> case readIntArray# array 0# state of
    (# next, value #) -> case value >=# 2# of
      0# -> case waitTwice (remaining -# 1#) of IO rest -> rest next
      _ -> (# next, () #)

-- The action holds its evaluation open until the other thread starts to
-- force the value, and for a while after that.
{-# NOINLINE shared #-}
shared :: Int
shared = unsafePerformIO $ do
  case runs of
    Counter array -> IO $ \state -> case fetchAddIntArray# array 0# 1# state of
      (# next, _ #) -> (# next, () #)
  waitFull 20000000#
  waitTwice 2000000#
  return 42

main :: IO ()
main = do
  _ <- evaluate runs
  _ <- evaluate entering
  first <- newEmptyMVar
  second <- newEmptyMVar
  _ <- forkIO (evaluate shared >>= putMVar first)
  _ <- forkIO (putMVar entering () >> evaluate shared >>= putMVar second)
  a <- takeMVar first
  b <- takeMVar second
  total <- count
  print (a, b, total)
