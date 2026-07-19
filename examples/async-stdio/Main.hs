module Main where

import GHC.IO.StdHandles (readStdinByte, writeStdoutByte)
import Foreign.C.Types (CInt)

-- This example deliberately uses the byte-level layer below Handle. It echoes
-- one byte while allowing the green-thread scheduler to run during either IO
-- operation.
main :: IO CInt
main = do
  byte <- readStdinByte
  writeStdoutByte byte
