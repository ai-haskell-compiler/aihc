{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.IO.Output (writeStderrString) where

import GHC.IO (IO (..))
import GHC.IO.Console (writeOutputByte, writeStderr)
import GHC.Internal.Char (Char (C#))
import GHC.Prim (MutableByteArray#, RealWorld, and#, int2Word#, newPinnedByteArray#, ord#, word2Int#, (+#), (==#))
import Prelude

writeStderrString :: String -> IO ()
writeStderrString characters = do
  buffer <- newStderrOutputBuffer 4096#
  case buffer of
    StderrOutputBuffer rawBuffer -> writeStderrStringChunks rawBuffer 0# (characters ++ "\n")

data StderrOutputBuffer = StderrOutputBuffer (MutableByteArray# RealWorld)

newStderrOutputBuffer :: Int# -> IO StderrOutputBuffer
newStderrOutputBuffer size =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, StderrOutputBuffer buffer #)
    )

writeStderrStringChunks :: MutableByteArray# RealWorld -> Int# -> String -> IO ()
writeStderrStringChunks buffer count characters =
  case characters of
    [] -> writeStderr buffer count
    character : remaining ->
      case (==#) count 4096# of
        1# -> do
          writeStderr buffer count
          writeStderrStringChunks buffer 0# characters
        _ -> do
          writeStderrCharacterByte buffer count character
          writeStderrStringChunks buffer ((+#) count 1#) remaining

writeStderrCharacterByte :: MutableByteArray# RealWorld -> Int# -> Char -> IO ()
writeStderrCharacterByte buffer offset (C# character) =
  writeOutputByte buffer offset (word2Int# (and# (int2Word# (ord# character)) (int2Word# 255#)))
