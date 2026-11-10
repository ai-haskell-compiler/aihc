module Closures (greet, failing) where

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
