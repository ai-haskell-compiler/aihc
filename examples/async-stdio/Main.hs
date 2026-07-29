{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
import GHC.Ptr (Ptr (..))
import System.IO (hGetBuf, hPutBuf, stdin, stdout)

-- This example uses one stable buffer through the Handle layer. It echoes one
-- input block while the green-thread scheduler can run during each IO request.
main :: IO ()
main =
  withExampleBuffer
    64
    ( \buffer -> do
        let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
        count <- hGetBuf stdin pointer 64
        hPutBuf stdout pointer count
    )

withExampleBuffer :: Int -> (MutableByteArray# RealWorld -> IO a) -> IO a
withExampleBuffer (I# size) action =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            case action buffer of
              IO run -> run allocatedState
    )
