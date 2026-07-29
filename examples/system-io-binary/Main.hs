{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (Addr#, MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
import GHC.Ptr (Ptr (..))
import System.IO
  ( IOMode (..),
    hClose,
    hGetBuf,
    hPutBuf,
    openBinaryFile,
    stderr,
    stdin,
    stdout,
  )

main :: IO ()
main = do
  output <- openBinaryFile "π" WriteMode
  hPutBuf output (Ptr "AB"# :: Ptr ()) 2
  hClose output

  appended <- openBinaryFile "π" AppendMode
  hPutBuf appended (Ptr "C"# :: Ptr ()) 1
  hClose appended

  inputBuffer <- newPinnedByteArray 3
  let inputPointer = Ptr (pinnedByteArrayContents# inputBuffer) :: Ptr ()
  inputFile <- openBinaryFile "π" ReadMode
  count <- hGetBuf inputFile inputPointer 3
  hClose inputFile
  hClose inputFile
  hGetBuf inputFile inputPointer 0
  hPutBuf stdout inputPointer count

  updated <- openBinaryFile "π" ReadWriteMode
  hPutBuf updated (Ptr "X"# :: Ptr ()) 1
  updatedBuffer <- newPinnedByteArray 2
  let updatedPointer = Ptr (pinnedByteArrayContents# updatedBuffer) :: Ptr ()
  updatedCount <- hGetBuf updated updatedPointer 2
  hPutBuf stdout updatedPointer updatedCount
  hClose updated

  hPutBuf stderr (Ptr "E\n"# :: Ptr ()) 2
  stdinBuffer <- newPinnedByteArray 2
  let stdinPointer = Ptr (pinnedByteArrayContents# stdinBuffer) :: Ptr ()
  stdinCount <- hGetBuf stdin stdinPointer 2
  hPutBuf stdout stdinPointer stdinCount
  hPutBuf stdout (Ptr "\n"# :: Ptr ()) 1

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
