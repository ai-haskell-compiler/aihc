{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The boxed-array primitives that move runs of elements between arrays,
-- and the small-array family that shares their representation.
module Main where

import GHC.Exts
import GHC.IO (IO (..))

data Immutable a = Immutable (Array# a)

data Mutable a = Mutable (MutableArray# RealWorld a)

data SmallImmutable a = SmallImmutable (SmallArray# a)

data SmallMutable a = SmallMutable (SmallMutableArray# RealWorld a)

new :: Int -> a -> IO (Mutable a)
new (I# count) initial =
  IO (\s -> case newArray# count initial s of (# s', array #) -> (# s', Mutable array #))

write :: Mutable a -> Int -> a -> IO ()
write (Mutable array) (I# index) value =
  IO (\s -> case writeArray# array index value s of s' -> (# s', () #))

readAt :: Mutable a -> Int -> IO a
readAt (Mutable array) (I# index) = IO (readArray# array index)

freeze :: Mutable a -> IO (Immutable a)
freeze (Mutable array) =
  IO (\s -> case unsafeFreezeArray# array s of (# s', frozen #) -> (# s', Immutable frozen #))

at :: Immutable a -> Int -> a
at (Immutable array) (I# index) = case indexArray# array index of (# element #) -> element

-- The five primitives under test on boxed arrays.
copyImmutable :: Immutable a -> Int -> Mutable a -> Int -> Int -> IO ()
copyImmutable (Immutable source) (I# from) (Mutable target) (I# to) (I# count) =
  IO (\s -> case copyArray# source from target to count s of s' -> (# s', () #))

copyMutable :: Mutable a -> Int -> Mutable a -> Int -> Int -> IO ()
copyMutable (Mutable source) (I# from) (Mutable target) (I# to) (I# count) =
  IO (\s -> case copyMutableArray# source from target to count s of s' -> (# s', () #))

clone :: Immutable a -> Int -> Int -> Immutable a
clone (Immutable source) (I# from) (I# count) = Immutable (cloneArray# source from count)

freezeRun :: Mutable a -> Int -> Int -> IO (Immutable a)
freezeRun (Mutable array) (I# from) (I# count) =
  IO (\s -> case freezeArray# array from count s of (# s', frozen #) -> (# s', Immutable frozen #))

thawRun :: Immutable a -> Int -> Int -> IO (Mutable a)
thawRun (Immutable array) (I# from) (I# count) =
  IO (\s -> case thawArray# array from count s of (# s', thawed #) -> (# s', Mutable thawed #))

sizeOf :: Immutable a -> Int
sizeOf (Immutable array) = I# (sizeofArray# array)

-- The small-array family.
newSmall :: Int -> a -> IO (SmallMutable a)
newSmall (I# count) initial =
  IO (\s -> case newSmallArray# count initial s of (# s', array #) -> (# s', SmallMutable array #))

writeSmall :: SmallMutable a -> Int -> a -> IO ()
writeSmall (SmallMutable array) (I# index) value =
  IO (\s -> case writeSmallArray# array index value s of s' -> (# s', () #))

readSmall :: SmallMutable a -> Int -> IO a
readSmall (SmallMutable array) (I# index) = IO (readSmallArray# array index)

cloneSmallMutable :: SmallMutable a -> Int -> Int -> IO (SmallMutable a)
cloneSmallMutable (SmallMutable array) (I# from) (I# count) =
  IO (\s -> case cloneSmallMutableArray# array from count s of (# s', cloned #) -> (# s', SmallMutable cloned #))

freezeSmall :: SmallMutable a -> IO (SmallImmutable a)
freezeSmall (SmallMutable array) =
  IO (\s -> case unsafeFreezeSmallArray# array s of (# s', frozen #) -> (# s', SmallImmutable frozen #))

atSmall :: SmallImmutable a -> Int -> a
atSmall (SmallImmutable array) (I# index) = case indexSmallArray# array index of (# element #) -> element

sizeOfSmall :: SmallImmutable a -> Int
sizeOfSmall (SmallImmutable array) = I# (sizeofSmallArray# array)

shrinkSmall :: SmallMutable a -> Int -> IO ()
shrinkSmall (SmallMutable array) (I# count) =
  IO (\s -> case shrinkSmallMutableArray# array count s of s' -> (# s', () #))

sizeOfSmallMutable :: SmallMutable a -> IO Int
sizeOfSmallMutable (SmallMutable array) =
  IO (\s -> case getSizeofSmallMutableArray# array s of (# s', count #) -> (# s', I# count #))

report :: String -> Bool -> IO ()
report label ok = putStrLn (label <> (if ok then ": ok" else ": FAILED"))

-- | @a b c d e@ as a five-element immutable array.
alphabet :: IO (Immutable Char)
alphabet = do
  array <- new 5 'a'
  write array 1 'b'
  write array 2 'c'
  write array 3 'd'
  write array 4 'e'
  freeze array

main :: IO ()
main = do
  source <- alphabet
  -- Copy a run out of an immutable array into a fresh mutable one.
  target <- new 5 'z'
  copyImmutable source 1 target 0 3
  copied <- freeze target
  report "copyArray#" (at copied 0 == 'b' && at copied 2 == 'd' && at copied 3 == 'z')

  -- Copy a run within one array, towards a higher index, so the source and
  -- the destination overlap.
  overlapping <- new 5 'z'
  copyImmutable source 0 overlapping 0 5
  copyMutable overlapping 0 overlapping 1 4
  shifted <- freeze overlapping
  report
    "copyMutableArray# overlapping"
    (at shifted 0 == 'a' && at shifted 1 == 'a' && at shifted 4 == 'd')

  let cloned = clone source 1 3
  report "cloneArray#" (sizeOf cloned == 3 && at cloned 0 == 'b' && at cloned 2 == 'd')

  whole <- new 5 'z'
  copyImmutable source 0 whole 0 5
  run <- freezeRun whole 2 2
  report "freezeArray#" (sizeOf run == 2 && at run 0 == 'c' && at run 1 == 'd')

  -- A thawed run is independent of the array it came from.
  thawed <- thawRun source 0 2
  write thawed 0 'x'
  thawedFirst <- readAt thawed 0
  report "thawArray#" (thawedFirst == 'x' && at source 0 == 'a')

  small <- newSmall 4 'p'
  writeSmall small 2 'q'
  smallClone <- cloneSmallMutable small 2 2
  clonedFirst <- readSmall smallClone 0
  frozenSmall <- freezeSmall small
  report
    "small arrays"
    (clonedFirst == 'q' && sizeOfSmall frozenSmall == 4 && atSmall frozenSmall 2 == 'q')

  shrinkable <- newSmall 6 'r'
  shrinkSmall shrinkable 2
  shrunk <- sizeOfSmallMutable shrinkable
  report "shrinkSmallMutableArray#" (shrunk == 2)
