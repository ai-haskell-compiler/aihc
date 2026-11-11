module Closures (greet, failing, tableSum, churn) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Foreign.C.String (withCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "aihc_engine_send" engineSend :: Ptr CChar -> Int -> IO ()

send :: String -> IO ()
send text = withCStringLen text (uncurry engineSend)

-- | The number of runs of 'greet'. The value stays in the instance.
runs :: IORef Int
runs = unsafePerformIO (newIORef 0)
{-# NOINLINE runs #-}

greet :: IO ()
greet = do
  modifyIORef' runs (+ 1)
  count <- readIORef runs
  send ("run " ++ show count)
  send (show (sum [1 .. 100000 :: Int]))

failing :: IO ()
failing = throwIO (ErrorCall "the closure failed")

-- | A static thunk. The first run of 'tableSum' evaluates it, and later
-- runs use its value again.
table :: [Int]
table = [1 .. 20000]
{-# NOINLINE table #-}

tableSum :: IO ()
tableSum = send (show (sum table))

-- | Allocate many objects that stay live for a short time. Thus, the
-- collector does full collections while no code refers to 'table'.
churn :: IO ()
churn = send (show (sum (map live [1 .. 12])))
  where
    -- The list stays live while 'length' walks it.
    live :: Int -> Int
    live seed = let values = [seed .. seed + 300000] in length values + sum values
