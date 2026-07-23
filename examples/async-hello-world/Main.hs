{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int (Int32 (..))
import Foreign.C.Types (CInt (..))
import GHC.IO.StdHandles
  ( copyAddrToIOBuffer,
    newIOBuffer,
    stdoutHandle,
    writeFromBuffer,
  )

main :: IO CInt
main = do
  buffer <- newIOBuffer length
  copyAddrToIOBuffer "Hello world!\n"# buffer zero length
  output <- stdoutHandle
  writeFromBuffer output buffer zero length
  where
    zero = CInt (I32# 0#Int32)
    length = CInt (I32# 13#Int32)
