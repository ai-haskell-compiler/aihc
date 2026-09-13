-- | Flat containers on unboxed arrays.
--
-- A compiler pass that runs on every function of every module cannot afford
-- one small container for each instruction. These types give the passes of
-- "Aihc.Lir.RegAlloc" what they would otherwise take from @containers@, with
-- no allocation for each element:
--
-- * 'Rows' is a list of lists in two arrays, and 'buildRows' makes one from
--   a walk that reports its entries in any order.
-- * 'IntBuffer' is a growable array, and 'Table' builds 'Rows' with it, one
--   row after another.
-- * 'Bits' is a set of numbers, or one set for each row.
--
-- The loops are monomorphic in 'ST' on purpose: a loop that is polymorphic
-- in the monad allocates a closure for each step.
module Aihc.Lir.Flat
  ( -- * Arrays
    boxedArray,
    newInts,
    newBools,
    newWords,
    freezeInts,
    lengthOf,

    -- * Loops
    forUpTo,
    forDownFrom,
    forEach,
    addTo,
    scanSums,

    -- * Rows
    Rows,
    rowFrom,
    rowTo,
    rowAt,
    rowsSize,
    forRow,
    buildRows,

    -- * Buffers
    IntBuffer,
    newIntBuffer,
    pushInt,
    bufferLength,
    freezeBufferWith,
    Table,
    newTable,
    openRow,
    pushValue,
    freezeTableWith,

    -- * Bits
    Bits,
    newBits,
    setBit,
    clearBit,
    clearRow,
    unionRow,
    replaceRow,
    forBits,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.Base (getNumElements)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (Array, UArray, bounds, listArray, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Data.Bits (complement, countTrailingZeros, shiftL, (.&.), (.|.))
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word64)

-- | These wrappers fix the array type, which the class of the operation
-- leaves open. Every array starts at index zero.
boxedArray :: Int -> [e] -> Array Int e
boxedArray size = listArray (0, size - 1)

{-# INLINE newInts #-}
newInts :: Int -> Int -> ST s (STUArray s Int Int)
newInts size = newArray (0, size - 1)

{-# INLINE newBools #-}
newBools :: Int -> Bool -> ST s (STUArray s Int Bool)
newBools size = newArray (0, size - 1)

{-# INLINE newWords #-}
newWords :: Int -> ST s (STUArray s Int Word64)
newWords size = newArray (0, size - 1) 0

{-# INLINE freezeInts #-}
freezeInts :: STUArray s Int Int -> ST s (UArray Int Int)
freezeInts = unsafeFreeze

{-# INLINE lengthOf #-}
lengthOf :: UArray Int Int -> Int
lengthOf array = snd (bounds array) + 1

-- | Run an action on every number below a limit, lowest first.
{-# INLINE forUpTo #-}
forUpTo :: Int -> (Int -> ST s ()) -> ST s ()
forUpTo limit act = go 0
  where
    go index = when (index < limit) (act index >> go (index + 1))

-- | Run an action on every number of a range, highest first.
{-# INLINE forDownFrom #-}
forDownFrom :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forDownFrom highest lowest act = go highest
  where
    go index = when (index >= lowest) (act index >> go (index - 1))

-- | Run an action on every element of a list, with its position.
{-# INLINE forEach #-}
forEach :: [a] -> (Int -> a -> ST s ()) -> ST s ()
forEach items act = go 0 items
  where
    go _ [] = pure ()
    go position (item : rest) = act position item >> go (position + 1) rest

{-# INLINE addTo #-}
addTo :: STUArray s Int Int -> Int -> Int -> ST s ()
addTo array index by = readArray array index >>= writeArray array index . (+ by)

-- | Turn counts into offsets, in place.
scanSums :: STUArray s Int Int -> Int -> ST s ()
scanSums array count = forUpTo count (\index -> readArray array index >>= addTo array (index + 1))

-- | Rows of values in one array: row @i@ holds the values from
-- @offsets ! i@ up to @offsets ! (i + 1)@.
data Rows = Rows !(UArray Int Int) !(UArray Int Int)

rowFrom, rowTo :: Rows -> Int -> Int
rowFrom (Rows offsets _) index = offsets ! index
rowTo (Rows offsets _) index = offsets ! (index + 1)

{-# INLINE rowAt #-}
rowAt :: Rows -> Int -> Int
rowAt (Rows _ values) at = values ! at

-- | How many values all the rows hold together.
{-# INLINE rowsSize #-}
rowsSize :: Rows -> Int
rowsSize (Rows _ values) = lengthOf values

{-# INLINE forRow #-}
forRow :: Rows -> Int -> (Int -> ST s ()) -> ST s ()
forRow rows index act = go (rowFrom rows index)
  where
    go at = when (at < rowTo rows index) (act (rowAt rows at) >> go (at + 1))

-- | Build rows from a walk that reports the row and the value of every
-- entry. The walk runs two times: it counts, and then it fills. Each row
-- keeps the order the walk reports its entries in.
{-# INLINE buildRows #-}
buildRows :: Int -> ((Int -> Int -> ST s ()) -> ST s ()) -> ST s Rows
buildRows rowCount walk = do
  offsets <- newInts (rowCount + 1) 0
  walk (\row _ -> addTo offsets (row + 1) 1)
  scanSums offsets rowCount
  cursor <- newInts (rowCount + 1) 0
  forUpTo (rowCount + 1) (\index -> readArray offsets index >>= writeArray cursor index)
  total <- readArray offsets rowCount
  values <- newInts total 0
  walk $ \row value -> do
    at <- readArray cursor row
    writeArray cursor row (at + 1)
    writeArray values at value
  Rows <$> freezeInts offsets <*> freezeInts values

-- | A growable array of integers. The count lives in the first element, so
-- a push writes no boxed value.
newtype IntBuffer s = IntBuffer (STRef s (STUArray s Int Int))

newIntBuffer :: Int -> ST s (IntBuffer s)
newIntBuffer capacity = IntBuffer <$> (newInts (max 4 capacity + 1) 0 >>= newSTRef)

{-# INLINE pushInt #-}
pushInt :: IntBuffer s -> Int -> ST s ()
pushInt (IntBuffer arrayRef) value = do
  array <- readSTRef arrayRef
  count <- readArray array 0
  capacity <- getNumElements array
  target <-
    if count + 1 < capacity
      then pure array
      else do
        bigger <- newInts (capacity * 2) 0
        forUpTo capacity (\index -> readArray array index >>= writeArray bigger index)
        writeSTRef arrayRef bigger
        pure bigger
  writeArray target (count + 1) value
  writeArray target 0 (count + 1)

{-# INLINE bufferLength #-}
bufferLength :: IntBuffer s -> ST s Int
bufferLength (IntBuffer arrayRef) = readSTRef arrayRef >>= (`readArray` 0)

-- | Freeze a buffer, and give the position and the value of each element to
-- a function on the way.
{-# INLINE freezeBufferWith #-}
freezeBufferWith :: (Int -> Int -> Int) -> IntBuffer s -> ST s (UArray Int Int)
freezeBufferWith rename buffer@(IntBuffer arrayRef) = do
  count <- bufferLength buffer
  array <- readSTRef arrayRef
  result <- newInts count 0
  forUpTo count (\index -> readArray array (index + 1) >>= writeArray result index . rename index)
  freezeInts result

-- | Leave a value as it is.
keep :: Int -> Int -> Int
keep _ value = value

-- | Rows under construction. The encoder opens each row in turn and pushes
-- the values of the row into it.
data Table s = Table !(IntBuffer s) !(IntBuffer s)

newTable :: Int -> Int -> ST s (Table s)
newTable rowCount valueCount = Table <$> newIntBuffer rowCount <*> newIntBuffer valueCount

{-# INLINE openRow #-}
openRow :: Table s -> ST s ()
openRow (Table offsets values) = bufferLength values >>= pushInt offsets

{-# INLINE pushValue #-}
pushValue :: Table s -> Int -> ST s ()
pushValue (Table _ values) = pushInt values

-- | Close the last row and freeze the table, with the values renamed.
freezeTableWith :: (Int -> Int) -> Table s -> ST s Rows
freezeTableWith rename table@(Table offsets values) = do
  openRow table
  Rows <$> freezeBufferWith keep offsets <*> freezeBufferWith (const rename) values

-- | One set of values for each row: one bit for each value, in rows of
-- @stride@ words.
data Bits s = Bits !(STUArray s Int Word64) !Int

newBits :: Int -> Int -> ST s (Bits s)
newBits rowCount valueCount = Bits <$> newWords (rowCount * stride) <*> pure stride
  where
    stride = max 1 ((valueCount + 63) `div` 64)

{-# INLINE changeBit #-}
changeBit :: (Word64 -> Word64 -> Word64) -> Bits s -> Int -> Int -> ST s ()
changeBit change (Bits array stride) row value = do
  let index = row * stride + (value `div` 64)
  word <- readArray array index
  writeArray array index (change word (1 `shiftL` (value `mod` 64)))

setBit, clearBit :: Bits s -> Int -> Int -> ST s ()
setBit = changeBit (.|.)
clearBit = changeBit (\word bit -> word .&. complement bit)

-- | Empty one row.
{-# INLINE clearRow #-}
clearRow :: Bits s -> Int -> ST s ()
clearRow (Bits array stride) row = forUpTo stride (\word -> writeArray array (row * stride + word) 0)

-- | Add every value of one row to another row.
{-# INLINE unionRow #-}
unionRow :: Bits s -> Int -> Bits s -> Int -> ST s ()
unionRow (Bits into stride) row (Bits from _) fromRow =
  forUpTo stride $ \word -> do
    there <- readArray from (fromRow * stride + word)
    here <- readArray into (row * stride + word)
    writeArray into (row * stride + word) (here .|. there)

-- | Copy one row over another, and say whether that row changed.
{-# INLINE replaceRow #-}
replaceRow :: Bits s -> Int -> Bits s -> Int -> ST s Bool
replaceRow (Bits into stride) row (Bits from _) fromRow = go 0 False
  where
    go word changed
      | word >= stride = pure changed
      | otherwise = do
          there <- readArray from (fromRow * stride + word)
          here <- readArray into (row * stride + word)
          when (here /= there) (writeArray into (row * stride + word) there)
          go (word + 1) (changed || here /= there)

-- | Run an action on every value of one row, lowest first.
{-# INLINE forBits #-}
forBits :: Bits s -> Int -> (Int -> ST s ()) -> ST s ()
forBits (Bits array stride) row act =
  forUpTo stride $ \word -> do
    bits <- readArray array (row * stride + word)
    values (word * 64) bits
  where
    values base bits
      | bits == 0 = pure ()
      | otherwise = act (base + countTrailingZeros bits) >> values base (bits .&. (bits - 1))
