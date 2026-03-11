{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (when)
import Data.Primitive.ByteArray
import Data.WideWord (Word128)
import Data.Word
import GHC.Exts (RealWorld)
import System.ByteOrder

import qualified Data.Primitive.ByteArray.BigEndian as BE

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  putStrLn "B"
  testB
  putStrLn "C"
  testC
  putStrLn "D"
  testD
  putStrLn "E"
  testE
  putStrLn "F"
  testF
  putStrLn "Finished"

testA :: IO ()
testA = do
  let payload = 0x01234567 :: Word32
  marr <- newByteArray 4
  setByteArray marr 0 4 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'LittleEndian payload)
  expectByte "testA, byte 0" marr 0 0x67
  expectByte "testA, byte 1" marr 1 0x45
  expectByte "testA, byte 2" marr 2 0x23
  expectByte "testA, byte 3" marr 3 0x01

testB :: IO ()
testB = do
  let payload = 0x01234567 :: Word32
  marr <- newByteArray 4
  setByteArray marr 0 4 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'BigEndian payload)
  let name = "testB"
  expectByte name marr 0 0x01
  expectByte name marr 1 0x23
  expectByte name marr 2 0x45
  expectByte name marr 3 0x67

testC :: IO ()
testC = do
  let payload = 0x0123456789ABCDEF :: Word64
  marr <- newByteArray 8
  setByteArray marr 0 8 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'BigEndian payload)
  let name = "testC"
  expectByte name marr 0 0x01
  expectByte name marr 1 0x23
  expectByte name marr 2 0x45
  expectByte name marr 3 0x67
  expectByte name marr 4 0x89
  expectByte name marr 5 0xAB
  expectByte name marr 6 0xCD
  expectByte name marr 7 0xEF

testD :: IO ()
testD = do
  let payload = 0x01234567 :: Word
  marr <- newByteArray 20
  setByteArray marr 0 20 (0x00 :: Word8)
  writeByteArray marr 0 (Fixed @'LittleEndian payload)
  let name = "testD"
  expectByte name marr 0 0x67
  expectByte name marr 1 0x45
  expectByte name marr 2 0x23
  expectByte name marr 3 0x01
  expectByte name marr 4 0x00

testE :: IO ()
testE = do
  marr <- newByteArray 8
  writeByteArray marr 0 (0xFF :: Word8)
  writeByteArray marr 1 (0xFF :: Word8)
  writeByteArray marr 2 (0xFF :: Word8)
  writeByteArray marr 3 (0xFF :: Word8)
  writeByteArray marr 4 (0x00 :: Word8)
  writeByteArray marr 5 (0x06 :: Word8)
  writeByteArray marr 6 (0x96 :: Word8)
  writeByteArray marr 7 (0x9c :: Word8)
  r <- BE.readByteArray marr 1
  let expected = 0x0006969c :: Word32
  when (r /= expected) (fail "testE failed")

testF :: IO ()
testF = do
  marr <- newByteArray 32
  writeByteArray marr 16 (0x00 :: Word8)
  writeByteArray marr 17 (0x01 :: Word8)
  writeByteArray marr 18 (0x02 :: Word8)
  writeByteArray marr 19 (0x03 :: Word8)
  writeByteArray marr 20 (0x04 :: Word8)
  writeByteArray marr 21 (0x05 :: Word8)
  writeByteArray marr 22 (0x06 :: Word8)
  writeByteArray marr 23 (0x07 :: Word8)
  writeByteArray marr 24 (0x08 :: Word8)
  writeByteArray marr 25 (0x09 :: Word8)
  writeByteArray marr 26 (0x0A :: Word8)
  writeByteArray marr 27 (0x0B :: Word8)
  writeByteArray marr 28 (0x0C :: Word8)
  writeByteArray marr 29 (0x0D :: Word8)
  writeByteArray marr 30 (0x0E :: Word8)
  writeByteArray marr 31 (0x0F :: Word8)
  r <- BE.readByteArray marr 1
  let expected = 0x000102030405060708090A0B0C0D0E0F :: Word128
  when (r /= expected) (fail "testF failed")

expectByte :: String -> MutableByteArray RealWorld -> Int -> Word8 -> IO ()
expectByte name marr ix w = do
  v <- readByteArray marr ix
  if v == w
    then pure ()
    else fail (name ++ ": byte " ++ show ix ++ " was wrong, expected " ++ show w ++ " but got " ++ show v)
