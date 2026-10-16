{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.List (foldl')
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize (..))

-- A safe foreign call gives up its capability. The arguments give the
-- machine two capabilities. One thread waits in a safe read of a pipe, and
-- only the other thread writes to the pipe. Before the write, the other
-- thread allocates enough to collect several times. A collection stops the
-- world, so it must not wait for the capability of the read. See "FFI" in
-- docs/multi-threaded-rts-design.md.

foreign import ccall unsafe "pipe" c_pipe :: Ptr CInt -> IO CInt

foreign import ccall safe "read" c_read :: CInt -> Ptr () -> CSize -> IO CSsize

foreign import ccall unsafe "write" c_write :: CInt -> Ptr () -> CSize -> IO CSsize

main :: IO ()
main = do
  descriptors <- allocaArray 2 $ \pair -> c_pipe pair >> peekArray 2 pair
  (input, output) <- case descriptors of
    [input, output] -> pure (input, output)
    _ -> fail "pipe gave no descriptors"
  started <- newEmptyMVar
  done <- newEmptyMVar
  _ <- forkIO $ allocaBytes 1 $ \buffer -> do
    putMVar started ()
    count <- c_read input buffer 1
    putMVar done count
  takeMVar started
  -- Each round builds a new list, so the rounds allocate about 200 MB.
  let total = foldl' (+) 0 [foldl' (+) 0 (map (* round) [1 .. 100000]) | round <- [1 .. 100 :: Int]]
  print total
  _ <- allocaBytes 1 $ \buffer -> c_write output buffer 1
  takeMVar done >>= print
