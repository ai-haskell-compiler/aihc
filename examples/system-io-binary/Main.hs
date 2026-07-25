{-# LANGUAGE MagicHash #-}

module Main where

import GHC.IO.StdHandles (withPinnedByteArray)
import GHC.Prim (mutableByteArrayContents#)
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

  withPinnedByteArray 3# (\buffer -> do
    let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
    inputFile <- openBinaryFile "π" ReadMode
    count <- hGetBuf inputFile pointer 3
    hClose inputFile
    hClose inputFile
    hGetBuf inputFile pointer 0
    hPutBuf stdout (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) count)

  updated <- openBinaryFile "π" ReadWriteMode
  hPutBuf updated (Ptr "X"# :: Ptr ()) 1
  withPinnedByteArray 2# (\buffer -> do
    let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
    count <- hGetBuf updated pointer 2
    hPutBuf stdout (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) count)
  hClose updated

  hPutBuf stderr (Ptr "E\n"# :: Ptr ()) 2
  withPinnedByteArray 2# (\buffer -> do
    let pointer = Ptr (mutableByteArrayContents# buffer) :: Ptr ()
    count <- hGetBuf stdin pointer 2
    hPutBuf stdout (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) count)
  hPutBuf stdout (Ptr "\n"# :: Ptr ()) 1
