{-# LANGUAGE ScopedTypeVariables #-}

-- | Asynchronous exceptions: masking, throwTo to the running thread and to
-- waiting threads, a sender that waits for a masked target, timeout of a
-- pure loop, and an interrupted thunk, on four capabilities. See
-- "Asynchronous exceptions" in docs/exceptions.md.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Maybe (isNothing)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

report :: MVar String -> IO () -> IO ()
report done action = action `catch` \(e :: AsyncException) -> putMVar done ("caught " ++ show e)

-- | The number of times the evaluation of 'spin' started.
{-# NOINLINE starts #-}
starts :: IORef Int
starts = unsafePerformIO (newIORef 0)

-- | The start value of the work. An 'IORef' gives it, so the shared thunk
-- below is a thunk of the local heap and not a static CAF. With several
-- capabilities, the runtime freezes only a thunk of the local heap.
{-# NOINLINE size #-}
size :: IORef Integer
size = unsafePerformIO (newIORef 0)

-- | The work stops when this flag is 'True'. Thus the work continues until
-- the timeout interrupts it, and the time of the work does not depend on
-- the speed of the machine.
{-# NOINLINE stop #-}
stop :: IORef Bool
stop = unsafePerformIO (newIORef False)

-- | Read the stop flag. The argument keeps the read in the loop.
{-# NOINLINE stopped #-}
stopped :: Integer -> Bool
stopped i = unsafePerformIO (readIORef stop) && i >= 0

-- | Work that continues until the flag 'stop' is 'True'.
{-# NOINLINE spin #-}
spin :: Integer -> Integer
spin n = unsafePerformIO (modifyIORef starts (+ 1)) `seq` go n
  where
    go i = if stopped i then i else go (i + 1)

main :: IO ()
main = do
  -- The masking states.
  print =<< getMaskingState
  mask_ (print =<< getMaskingState)
  uninterruptibleMask_ (print =<< getMaskingState)
  mask (\restore -> restore (print =<< getMaskingState))
  throwIO (userError "handler") `catch` \(_ :: IOException) -> print =<< getMaskingState
  print =<< getMaskingState

  -- A throwTo to the running thread raises at once.
  me <- myThreadId
  throwTo me ThreadKilled `catch` \(e :: AsyncException) -> putStrLn ("self: " ++ show e)

  -- A thread that waits in an MVar takes the exception.
  done <- newEmptyMVar
  box <- newEmptyMVar :: IO (MVar ())
  ready <- newEmptyMVar
  waiter <- forkIO $ report done (putMVar ready () >> takeMVar box)
  takeMVar ready
  killThread waiter
  putStrLn =<< takeMVar done

  -- An uninterruptible mask holds through a wait. The exception comes at
  -- the unmask, and the sender waits until then.
  gate <- newEmptyMVar
  events <- newMVar []
  let note s = modifyMVar_ events (pure . (s :))
  masked <- forkIO $ report done $ do
    uninterruptibleMask_ (putMVar ready () >> takeMVar gate >> note "masked work finished")
    note "not reached"
  takeMVar ready
  _ <- forkIO (threadDelay 20000 >> note "release" >> putMVar gate ())
  killThread masked
  note "kill returned"
  putStrLn =<< takeMVar done
  mapM_ putStrLn . reverse =<< readMVar events

  -- An interruptible mask lets a wait take the exception.
  interruptibleWaiter <- forkIO $ report done $ mask_ (putMVar ready () >> takeMVar box)
  takeMVar ready
  killThread interruptibleWaiter
  putStrLn =<< takeMVar done

  -- A throwTo to a finished thread returns.
  finished <- forkIO (pure ())
  threadDelay 1000
  killThread finished
  putStrLn "finished target"

  -- bracket releases on an asynchronous exception.
  released <- newEmptyMVar
  started <- newEmptyMVar
  user <- forkIO $ bracket (pure ()) (\_ -> putMVar released "released") (\_ -> putMVar started () >> threadDelay 10000000)
  takeMVar started
  killThread user
  putStrLn =<< takeMVar released

  -- timeout stops a pure loop.
  print =<< timeout 20000 (evaluate (length [1 :: Integer ..]))
  print =<< timeout 1000000 (pure (42 :: Int))

  -- An interrupted evaluation is frozen. The next evaluation continues it
  -- and does not start it again.
  n <- readIORef size
  let shared = spin n
  first <- timeout 20000 (evaluate shared)
  print (isNothing first)
  writeIORef stop True
  print . (>= n) =<< evaluate shared
  -- With several capabilities, a global thunk goes back to its code, and a
  -- thread that another capability takes can make the thunk global. Thus
  -- the work started once or twice.
  print . (`elem` [1, 2]) =<< readIORef starts
