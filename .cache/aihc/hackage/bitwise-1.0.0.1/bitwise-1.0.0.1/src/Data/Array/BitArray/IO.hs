{-|

Module      :  Data.Array.BitArray.IO
Copyright   :  (c) Claude Heiland-Allen 2012,2018
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  portable

Unboxed mutable bit arrays in the 'IO' monad.

-}
module Data.Array.BitArray.IO
  ( IOBitArray()
  -- * MArray-like interface.
  , getBounds
  , newArray
  , newArray_
  , newListArray
  , readArray
  , writeArray
  , mapArray
  , mapIndices
  , getElems
  , getAssocs
  -- * Conversion to/from immutable bit arrays.
  , freeze
  , thaw
  -- * Construction
  , copy
  , fill
  -- * Short-circuiting reductions.
  , or
  , and
  , isUniform
  , elemIndex
  -- * Aggregate operations.
  , fold
  , map
  , zipWith
  , popCount
  -- * Unsafe.
  , unsafeReadArray
  , unsafeGetElems
  , unsafeFreeze
  , unsafeThaw
  ) where

import Prelude hiding (and, or, map, zipWith)

import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, testBit, setBit, clearBit, (.&.), complement)
import qualified Data.Bits
import Data.Ix (Ix, index, inRange, range, rangeSize)
import Data.List (foldl1')
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr, touchForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (poke, pokeByteOff, pokeElemOff, peekByteOff, peekElemOff)
import System.IO.Unsafe (unsafeInterleaveIO)

import Data.Bits.Bitwise (packWord8LE, mask)
import qualified Data.Bits.Bitwise as Bitwise

import Data.Array.BitArray.Internal
  ( IOBitArray(..)
  , getBounds
  , newArray_
  , freeze
  , unsafeFreeze
  , thaw
  , unsafeThaw
  , copy
  )

-- | Create a new array filled with an initial value.
{-# INLINE newArray #-}
newArray :: Ix i => (i, i) {- ^ bounds -} -> Bool {- ^ initial value -} -> IO (IOBitArray i)
newArray bs b = do
  a <- newArray_ bs
  fill a b
  return a

-- | Create a new array filled with values from a list.
{-# INLINE newListArray #-}
newListArray :: Ix i => (i, i) {- ^ bounds -} -> [Bool] {- ^ elems -} -> IO (IOBitArray i)
newListArray bs es = do
  a <- newArray_ bs
  let byteBits = 8
      writeBytes :: Ptr Word8 -> [Bool] -> IO ()
      writeBytes p (b0:b1:b2:b3:b4:b5:b6:b7:rest) = do
        poke p (packWord8LE b0 b1 b2 b3 b4 b5 b6 b7)
        writeBytes (plusPtr p 1) rest
      writeBytes _ [] = return ()
      writeBytes p rest = writeBytes p (take byteBits (rest ++ repeat False))
  withForeignPtr (iobData a) $ \p -> do
    writeBytes (castPtr p) (take (byteBits * iobBytes a) es)
  return a

-- | Read from an array at an index.
{-# INLINE readArray #-}
readArray :: Ix i => IOBitArray i -> i -> IO Bool
readArray a i = do
  bs <- getBounds a
  when (not (inRange bs i)) $ error "array index out of bounds"
  readArrayRaw a (index bs i)

-- | Read from an array at an index without bounds checking.  Unsafe.
{-# INLINE unsafeReadArray #-}
unsafeReadArray :: Ix i => IOBitArray i -> i -> IO Bool
unsafeReadArray a i = do
  bs <- getBounds a
  readArrayRaw a (index bs i)

{-# INLINE readArrayRaw #-}
readArrayRaw :: Ix i => IOBitArray i -> Int -> IO Bool
readArrayRaw a n = do
  let byte = n `shiftR` 3
      bit = n .&. 7
  withForeignPtr (iobData a) $ \p -> do
    b0 <- peekByteOff p byte
    return (testBit (b0 :: Word8) bit)

-- | Write to an array at an index.
{-# INLINE writeArray #-}
writeArray :: Ix i => IOBitArray i -> i -> Bool -> IO ()
writeArray a i b = do
  bs <- getBounds a
  when (not (inRange bs i)) $ error "array index out of bounds"
  let n = index bs i
      byte = n `shiftR` 3
      bit = n .&. 7
  withForeignPtr (iobData a) $ \p -> do
    b0 <- peekByteOff p byte
    let b1 = (if b then setBit else clearBit) (b0 :: Word8) bit
    pokeByteOff p byte b1

-- | Alias for 'map'.
{-# INLINE mapArray #-}
mapArray :: Ix i => (Bool -> Bool) -> IOBitArray i -> IO (IOBitArray i)
mapArray = map

-- unsafeInterleaveIO is used to avoid having to create the whole list in
-- memory before the function can return, but need to keep the ForeignPtr
-- alive to avoid GC stealing our data.
interleavedMapMThenTouch :: Ix i => IOBitArray i -> (a -> IO b) -> [a] -> IO [b]
interleavedMapMThenTouch a _ [] = touchForeignPtr (iobData a) >> return []
interleavedMapMThenTouch a f (x:xs) = unsafeInterleaveIO $ do
  y <- f x
  ys <- interleavedMapMThenTouch a f xs
  return (y:ys)

-- | Create a new array by reading from another.
{-# INLINE mapIndices #-}
mapIndices :: (Ix i, Ix j) => (i, i) {- ^ new bounds -} -> (i -> j) {- ^ index transformation -} -> IOBitArray j {- ^ source array -} -> IO (IOBitArray i)
mapIndices bs h a = newListArray bs =<< interleavedMapMThenTouch a (readArray a . h) (range bs)

-- | Get a list of all elements of an array.
{-# INLINE getElems #-}
getElems :: Ix i => IOBitArray i -> IO [Bool]
getElems a = unsafeGetElems =<< copy a

-- | Get a list of all elements of an array.  Unsafe when the source
--   array can be modified later.
{-# INLINE unsafeGetElems #-}
unsafeGetElems :: Ix i => IOBitArray i -> IO [Bool]
unsafeGetElems a' = do
  bs <- getBounds a'
  let r = rangeSize bs
      count = (r + 7) `shiftR` 3
  p <- withForeignPtr (iobData a') $ return
  bytes <- interleavedMapMThenTouch a' (peekByteOff p) [0 .. count - 1]
  return . take r . concatMap Bitwise.toListLE $ (bytes :: [Word8])

-- | Get a list of all (index, element) pairs.
{-# INLINE getAssocs #-}
getAssocs :: Ix i => IOBitArray i -> IO [(i, Bool)]
getAssocs a = do
  bs <- getBounds a
  zip (range bs) `fmap` getElems a
  
-- | Fill an array with a uniform value.
{-# INLINE fill #-}
fill :: Ix i => IOBitArray i -> Bool -> IO ()
fill a b = do
  let count = iobBytes a `shiftR` 3
      word :: Word64
      word = if b then complement 0 else 0
  withForeignPtr (iobData a) $ \p ->
    forM_ [0 .. count - 1] $ \i ->
      pokeElemOff p i word

-- | Short-circuit bitwise reduction: True when any bit is True.
{-# INLINE or #-}
or :: Ix i => IOBitArray i -> IO Bool
or a = do
  bs <- getBounds a
  let total = rangeSize bs
      full = total .&. complement (mask 6)
      count = full `shiftR` 6
      loop :: Ptr Word64 -> Int -> IO Bool
      loop p n
        | n < count = do
            w <- peekElemOff p n
            if w /= (0 :: Word64) then return True else loop p (n + 1)
        | otherwise = rest full
      rest m
        | m < total = do
            b <- readArrayRaw a m
            if b then return True else rest (m + 1)
        | otherwise = return False
  withForeignPtr (iobData a) $ \p -> loop p 0

-- | Short-circuit bitwise reduction: False when any bit is False.
{-# INLINE and #-}
and :: Ix i => IOBitArray i -> IO Bool
and a = do
  bs <- getBounds a
  let total = rangeSize bs
      full = total .&. complement (mask 6)
      count = full `shiftR` 6
      loop :: Ptr Word64 -> Int -> IO Bool
      loop p n
        | n < count = do
            w <- peekElemOff p n
            if w /= (complement 0 :: Word64) then return False else loop p (n + 1)
        | otherwise = rest full
      rest m
        | m < total = do
            b <- readArrayRaw a m
            if not b then return False else rest (m + 1)
        | otherwise = return True
  withForeignPtr (iobData a) $ \p -> loop p 0

-- | Short-circuit bitwise reduction: 'Nothing' when any bits differ,
--   'Just' when all bits are the same.
{-# INLINE isUniform #-}
isUniform :: Ix i => IOBitArray i -> IO (Maybe Bool)
isUniform a = do
  bs <- getBounds a
  let total = rangeSize bs
      full = total .&. complement (mask 6)
      count = full `shiftR` 6
      loop :: Ptr Word64 -> Int -> Bool -> Bool -> IO (Maybe Bool)
      loop p n st sf
        | n < count = do
            w <- peekElemOff p n
            let t = w /= (0 :: Word64)  || st
                f = w /= (complement 0) || sf
            if t && f then return Nothing else loop p (n + 1) t f
        | otherwise = rest full st sf
      rest m st sf
        | m < total = do
            b <- readArrayRaw a m
            let t =     b || st
                f = not b || sf
            if t && f then return Nothing else rest (m + 1) t f
        | st && not sf = return (Just True)
        | not st && sf = return (Just False)
        | otherwise = return Nothing
  withForeignPtr (iobData a) $ \p -> loop p 0 False False

-- | Look up index of first matching bit.
--
--   Note that the index type is limited to Int because there
--   is no 'unindex' method in the 'Ix' class.
{-# INLINE elemIndex #-}
elemIndex :: Bool -> IOBitArray Int -> IO (Maybe Int)
elemIndex which a = do
  bs <- getBounds a
  let skip :: Word64
      skip | which = 0
           | otherwise = complement 0
      total = rangeSize bs
      full = total .&. complement (mask 6)
      count = full `shiftR` 6
      loop :: Ptr Word64 -> Int -> IO (Maybe Int)
      loop p n
        | n < count = do
            w <- peekElemOff p n
            if w /= skip then rest (n `shiftL` 6) else loop p (n + 1)
        | otherwise = rest full
      rest m
        | m < total = do
            b <- readArrayRaw a m
            if b == which then return (Just (fst bs + m)) else rest (m + 1)
        | otherwise = return Nothing
  withForeignPtr (iobData a) $ \p -> loop p 0

-- | Bitwise reduction with an associative commutative boolean operator.
--   Implementation lifts from 'Bool' to 'Bits' and folds large chunks
--   at a time.  Each bit is used as a source exactly once.
{-# INLINE fold #-}
fold :: Ix i => (Bool -> Bool -> Bool) {- ^ operator -} -> IOBitArray i -> IO (Maybe Bool)
fold f a = do
  bs <- getBounds a
  let g = Bitwise.zipWith f
      total = rangeSize bs
      full = total .&. complement (mask 6)
      count = full `shiftR` 6
      loop :: Ptr Word64 -> Int -> Maybe Word64 -> IO (Maybe Bool)
      loop p n mw
        | n < count = do
            w <- peekElemOff p n
            case mw of
              Nothing -> loop p (n + 1) (Just $!      w)
              Just w0 -> loop p (n + 1) (Just $! g w0 w)
        | otherwise =
            case mw of
              Nothing -> rest full Nothing
              Just w0 -> rest full (Just $! foldl1' f (Bitwise.toListLE w0))
      rest m mb
        | m < total = do
            b <- readArrayRaw a m
            case mb of
              Nothing -> rest (m + 1) (Just $!      b)
              Just b0 -> rest (m + 1) (Just $! f b0 b)
        | otherwise = return mb
  withForeignPtr (iobData a) $ \p -> loop p 0 Nothing

-- | Bitwise map.  Implementation lifts from 'Bool' to 'Bits' and maps
--   large chunks at a time.
{-# INLINE map #-}
map :: Ix i => (Bool -> Bool) -> IOBitArray i -> IO (IOBitArray i)
map f a = do
  bs <- getBounds a
  b <- newArray_ bs
  mapTo b f a
  return b

{-# INLINE mapTo #-}
mapTo :: Ix i => IOBitArray i -> (Bool -> Bool) -> IOBitArray i -> IO ()
mapTo dst f src = do
  -- {
  sbs <- getBounds src
  dbs <- getBounds dst
  when (sbs /= dbs) $ error "mapTo mismatched bounds"
  -- }
  let count = iobBytes dst `shiftR` 3
      g :: Word64 -> Word64
      g = Bitwise.map f
  withForeignPtr (iobData src) $ \sp ->
    withForeignPtr (iobData dst) $ \dp ->
      forM_ [0 .. count - 1] $ \n -> do
        pokeElemOff dp n . g =<< peekElemOff sp n

-- | Bitwise zipWith.  Implementation lifts from 'Bool' to 'Bits' and
--   combines large chunks at a time.
--
--   The bounds of the source arrays must be identical.
{-# INLINE zipWith #-}
zipWith :: Ix i => (Bool -> Bool -> Bool) -> IOBitArray i -> IOBitArray i -> IO (IOBitArray i)
zipWith f l r = do
  lbs <- getBounds l
  rbs <- getBounds r
  when (lbs /= rbs) $ error "zipWith mismatched bounds"
  c <- newArray_ lbs
  zipWithTo c f l r
  return c

{-# INLINE zipWithTo #-}
zipWithTo :: Ix i => IOBitArray i -> (Bool -> Bool -> Bool) -> IOBitArray i -> IOBitArray i -> IO ()
zipWithTo dst f l r = do
  lbs <- getBounds l
  rbs <- getBounds r
  dbs <- getBounds dst
  when (lbs /= rbs || dbs /= lbs || dbs /= rbs) $ error "zipWithTo mismatched bounds"
  let count = iobBytes dst `shiftR` 3
      g :: Word64 -> Word64 -> Word64
      g = Bitwise.zipWith f
  withForeignPtr (iobData l) $ \lp ->
    withForeignPtr (iobData r) $ \rp ->
      withForeignPtr (iobData dst) $ \dp ->
          forM_ [0 .. count - 1] $ \n -> do
            p <- peekElemOff lp n
            q <- peekElemOff rp n
            pokeElemOff dp n (g p q)

-- | Count set bits.
{-# INLINE popCount #-}
popCount :: Ix i => IOBitArray i -> IO Int
popCount a = do
  bs <- getBounds a
  let total = rangeSize bs
      full = total .&. complement (mask 6)
      count = full `shiftR` 6
      loop :: Ptr Word64 -> Int -> Int -> IO Int
      loop p n acc
        | n < count = acc `seq` do
            w <- peekElemOff p n
            loop p (n + 1) (acc + Data.Bits.popCount w)
        | otherwise = rest full acc
      rest m acc
        | m < total = acc `seq` do
            b <- readArrayRaw a m
            rest (m + 1) (acc + fromEnum b)
        | otherwise = return acc
  withForeignPtr (iobData a) $ \p -> loop p 0 0
