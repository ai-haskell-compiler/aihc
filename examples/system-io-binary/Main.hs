{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
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

  withExampleBuffer
    3
    ( \buffer -> do
        let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
        inputFile <- openBinaryFile "π" ReadMode
        count <- hGetBuf inputFile pointer 3
        hClose inputFile
        hClose inputFile
        hGetBuf inputFile pointer 0
        hPutBuf stdout (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) count
    )

  updated <- openBinaryFile "π" ReadWriteMode
  hPutBuf updated (Ptr "X"# :: Ptr ()) 1
  withExampleBuffer
    2
    ( \buffer -> do
        let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
        count <- hGetBuf updated pointer 2
        hPutBuf stdout (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) count
    )
  hClose updated

  hPutBuf stderr (Ptr "E\n"# :: Ptr ()) 2
  withExampleBuffer
    2
    ( \buffer -> do
        let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
        count <- hGetBuf stdin pointer 2
        hPutBuf stdout (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) count
    )
  hPutBuf stdout (Ptr "\n"# :: Ptr ()) 1

withExampleBuffer :: Int -> (MutableByteArray# RealWorld -> IO a) -> IO a
withExampleBuffer (I# size) action =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            case action buffer of
              IO run -> run allocatedState
    )
