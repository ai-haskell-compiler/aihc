{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (Addr#, MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
import GHC.Ptr (Ptr (..))
import System.IO (hGetBuf, hPutBuf, stdin, stdout)

-- This example uses one stable buffer through the Handle layer. It echoes one
-- input block while the green-thread scheduler can run during each IO request.
main :: IO ()
main = do
  buffer <- newPinnedByteArray 64
  let pointer = Ptr (pinnedByteArrayContents# buffer) :: Ptr ()
  count <- hGetBuf stdin pointer 64
  hPutBuf stdout pointer count

data PinnedByteArray = PinnedByteArray (MutableByteArray# RealWorld)

pinnedByteArrayContents# :: PinnedByteArray -> Addr#
pinnedByteArrayContents# (PinnedByteArray buffer) = mutableByteArrayContents# buffer

newPinnedByteArray :: Int -> IO PinnedByteArray
newPinnedByteArray (I# size) =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, PinnedByteArray buffer #)
    )
