{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Control.Monad (forM_, unless)
import GHC.Exts
import GHC.IO (IO (..))

-- A GC workload: objects at or above the large object bound of the runtime,
-- 32 KiB. Three such objects stay live for the whole run, and each round
-- drops two more. The live boxed array receives a fresh small object in
-- every round, so a collection finds young pointers in an object that never
-- moves.
data Boxed = Boxed (MutableArray# RealWorld Int)

data Bytes = Bytes (MutableByteArray# RealWorld)

newBoxed :: Int -> IO Boxed
newBoxed (I# count) =
  IO (\s -> case newArray# count 0 s of (# s', array #) -> (# s', Boxed array #))

writeBoxed :: Boxed -> Int -> Int -> IO ()
writeBoxed (Boxed array) (I# index) value =
  IO (\s -> case writeArray# array index value s of s' -> (# s', () #))

readBoxed :: Boxed -> Int -> IO Int
readBoxed (Boxed array) (I# index) = IO (readArray# array index)

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

byteSize :: Bytes -> IO Int
byteSize (Bytes array) =
  IO (\s -> case getSizeofMutableByteArray# array s of (# s', size #) -> (# s', I# size #))

isPinned :: Bytes -> Bool
isPinned (Bytes array) = isTrue# (isMutableByteArrayPinned# array)

boxedCount, intCount, rounds :: Int
boxedCount = 8192
intCount = 8192
rounds = 200

main :: IO ()
main = do
  boxed <- newBoxed boxedCount
  plain <- newBytes (intCount * 8) False
  pinned <- newBytes (intCount * 8) True
  forM_ [0 .. intCount - 1] $ \index -> do
    writeInt plain index index
    writeInt pinned index (negate index)
  forM_ [1 .. rounds] $ \round' -> do
    -- Two large objects that die at the end of the round.
    garbageBoxed <- newBoxed 4096
    garbageBytes <- newBytes 40000 (odd round')
    writeBoxed garbageBoxed 7 round'
    writeInt garbageBytes 7 round'
    kept <- readBoxed garbageBoxed 7
    kept' <- readInt garbageBytes 7
    unless (kept == round' && kept' == round') (fail "garbage object lost its field")
    -- Fresh small objects into the live large array.
    forM_ [0 .. 63] $ \slot ->
      writeBoxed boxed ((round' * 64 + slot) `mod` boxedCount) (round' * slot)
  total <- sum <$> mapM (readBoxed boxed) [0 .. boxedCount - 1]
  print total
  plainTotal <- sum <$> mapM (readInt plain) [0 .. intCount - 1]
  pinnedTotal <- sum <$> mapM (readInt pinned) [0 .. intCount - 1]
  print (plainTotal, pinnedTotal)
  plainSize <- byteSize plain
  pinnedSize <- byteSize pinned
  print (plainSize, pinnedSize, isPinned plain, isPinned pinned)
