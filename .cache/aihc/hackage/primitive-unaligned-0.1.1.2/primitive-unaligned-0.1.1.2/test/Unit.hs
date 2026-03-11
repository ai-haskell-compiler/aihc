{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

import Control.Monad (when)
import Data.Word
import Data.Primitive.ByteArray
import Data.Primitive.ByteArray.Unaligned

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  putStrLn "Finished"

testA :: IO ()
testA = do
  let expected = 0x0123456789ABCDEF :: Word64
  marr <- newByteArray 16
  setByteArray marr 0 16 (0x00 :: Word8)
  writeUnalignedByteArray marr 3 expected
  actualX :: Word64 <- readUnalignedByteArray marr 3
  when (expected /= actualX) $ fail "testA"
  arr <- unsafeFreezeByteArray marr
  let actualY = indexUnalignedByteArray arr 3
  when (expected /= actualY) $ fail "testA"
