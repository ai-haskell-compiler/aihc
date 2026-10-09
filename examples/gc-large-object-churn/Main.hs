{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Control.Monad (foldM, forM_, unless)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Exts
import GHC.IO (IO (..))

-- A GC workload: one live large object that the program replaces in each
-- round. A mutable reference holds a byte array of 1 MiB, well above the
-- large object bound of the runtime, 32 KiB. Each round allocates a fresh
-- array and drops the old array. Live data stays below 3 MiB, but the
-- rounds allocate 600 MiB. A minor collection promotes the live array, so
-- most dropped arrays are old when they die. The file max-peak-heap-bytes
-- sets the maximum heap peak. A collector that frees the old dead arrays
-- only at the -M limit goes above it. A large pinned array with known
-- content stays live for the whole run, so a collector that frees live
-- pinned data gives a wrong result.
data Bytes = Bytes (MutableByteArray# RealWorld)

newBytes :: Int -> Bool -> IO Bytes
newBytes (I# bytes) pinned =
  IO
    ( \s -> case (if pinned then newPinnedByteArray# bytes s else newByteArray# bytes s) of
        (# s', array #) -> (# s', Bytes array #)
    )

writeInt :: Bytes -> Int -> Int -> IO ()
writeInt (Bytes array) (I# index) (I# value) =
  IO (\s -> case writeIntArray# array index value s of s' -> (# s', () #))

readInt :: Bytes -> Int -> IO Int
readInt (Bytes array) (I# index) =
  IO (\s -> case readIntArray# array index s of (# s', value #) -> (# s', I# value #))

isPinned :: Bytes -> Bool
isPinned (Bytes array) = isTrue# (isMutableByteArrayPinned# array)

-- The churn array holds churnInts Ints. Each round writes one Int in each
-- block of stride Ints, so every page of the array gets a write.
churnInts, stride, touched, rounds, pinnedInts :: Int
churnInts = 131072
stride = 512
touched = churnInts `div` stride
rounds = 600
pinnedInts = 32768

cellValue :: Int -> Int -> Int
cellValue round' slot = round' * 7919 + slot * 31

-- A fresh churn array with the values of one round.
churnArray :: Int -> IO Bytes
churnArray round' = do
  array <- newBytes (churnInts * 8) False
  forM_ [0 .. touched - 1] $ \slot ->
    writeInt array (slot * stride) (cellValue round' slot)
  pure array

touchedSum :: Bytes -> IO Int
touchedSum array =
  foldM (\acc slot -> (acc +) <$> readInt array (slot * stride)) 0 [0 .. touched - 1]

main :: IO ()
main = do
  pinned <- newBytes (pinnedInts * 8) True
  forM_ [0 .. pinnedInts - 1] $ \index ->
    writeInt pinned index (index * 3 + 1)
  first <- churnArray 0
  live <- newIORef first
  total <-
    foldM
      ( \acc round' -> do
          -- Check the live array before the program drops it.
          old <- readIORef live
          oldSum <- touchedSum old
          let expected = sum (map (cellValue (round' - 1)) [0 .. touched - 1])
          unless (oldSum == expected) (fail ("live array lost its content in round " ++ show round'))
          fresh <- churnArray round'
          writeIORef live fresh
          pure $! acc + oldSum
      )
      0
      [1 .. rounds]
  print total
  final <- readIORef live
  touchedSum final >>= print
  firstCell <- readInt final 0
  lastCell <- readInt final ((touched - 1) * stride)
  print (firstCell, lastCell)
  pinnedSum <- foldM (\acc index -> (acc +) <$> readInt pinned index) 0 [0 .. pinnedInts - 1]
  print (pinnedSum, isPinned pinned)
