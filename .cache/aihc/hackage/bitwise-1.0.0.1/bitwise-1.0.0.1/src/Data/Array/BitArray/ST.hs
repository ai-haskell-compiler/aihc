{-|

Module      :  Data.Array.BitArray.ST
Copyright   :  (c) Claude Heiland-Allen 2012,2018
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  uses ST

Unboxed mutable bit arrays in the 'ST' monad.

-}
-- almost all is implemented with unsafeIOToST and the IO-based implementation
module Data.Array.BitArray.ST
  ( STBitArray()
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
  -- * Construction.
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
import Control.Monad.ST (ST)
import Data.Ix (Ix)

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Array.BitArray.Internal (BitArray)
import Data.Array.BitArray.IO (IOBitArray)
import qualified Data.Array.BitArray.IO as IO

-- | The type of mutable bit arrays.
newtype STBitArray s i = STB (IOBitArray i)

-- | Get the bounds of a bit array.
{-# INLINE getBounds #-}
getBounds :: Ix i => STBitArray s i -> ST s (i, i)
getBounds (STB a) = unsafeIOToST (IO.getBounds a)

-- | Create a new array filled with an initial value.
{-# INLINE newArray #-}
newArray :: Ix i => (i, i) {- ^ bounds -} -> Bool {- ^ initial value -} -> ST s (STBitArray s i)
newArray bs b = STB `fmap` unsafeIOToST (IO.newArray bs b)

-- | Create a new array filled with a default initial value ('False').
{-# INLINE newArray_ #-}
newArray_ :: Ix i => (i, i) {- ^ bounds -} -> ST s (STBitArray s i)
newArray_ bs = STB `fmap` unsafeIOToST (IO.newArray bs False)

-- | Create a new array filled with values from a list.
{-# INLINE newListArray #-}
newListArray :: Ix i => (i, i) {- ^ bounds -} -> [Bool] {- ^ elems -} -> ST s (STBitArray s i)
newListArray bs es = STB `fmap` unsafeIOToST (IO.newListArray bs es)

-- | Read from an array at an index.
{-# INLINE readArray #-}
readArray :: Ix i => STBitArray s i -> i -> ST s Bool
readArray (STB a) i = unsafeIOToST (IO.readArray a i)

-- | Read from an array at an index without bounds checking.  Unsafe.
{-# INLINE unsafeReadArray #-}
unsafeReadArray :: Ix i => STBitArray s i -> i -> ST s Bool
unsafeReadArray (STB a) i = unsafeIOToST (IO.unsafeReadArray a i)

-- | Write to an array at an index.
{-# INLINE writeArray #-}
writeArray :: Ix i => STBitArray s i -> i -> Bool -> ST s ()
writeArray (STB a) i b = unsafeIOToST (IO.writeArray a i b)

-- | Alias for 'map'.
{-# INLINE mapArray #-}
mapArray :: Ix i => (Bool -> Bool) -> STBitArray s i -> ST s (STBitArray s i)
mapArray = map

-- | Create a new array by reading from another.
{-# INLINE mapIndices #-}
mapIndices :: (Ix i, Ix j) => (i, i) {- ^ new bounds -} -> (i -> j) {- ^ index transformation -} -> STBitArray s j {- ^ source array -} -> ST s (STBitArray s i)
mapIndices bs h (STB a) = STB `fmap` unsafeIOToST (IO.mapIndices bs h a)

-- | Get a list of all elements of an array.
{-# INLINE getElems #-}
getElems :: Ix i => STBitArray s i -> ST s [Bool]
getElems (STB a) = unsafeIOToST (IO.getElems a)

-- | Get a list of all elements of an array without copying.  Unsafe when
--   the source array can be modified later.
{-# INLINE unsafeGetElems #-}
unsafeGetElems :: Ix i => STBitArray s i -> ST s [Bool]
unsafeGetElems (STB a) = unsafeIOToST (IO.unsafeGetElems a)

-- | Get a list of all (index, element) pairs.
{-# INLINE getAssocs #-}
getAssocs :: Ix i => STBitArray s i -> ST s [(i, Bool)]
getAssocs (STB a) = unsafeIOToST (IO.getAssocs a)

-- | Snapshot the array into an immutable form.
{-# INLINE freeze #-}
freeze :: Ix i => STBitArray s i -> ST s (BitArray i)
freeze (STB a) = unsafeIOToST (IO.freeze a)

-- | Snapshot the array into an immutable form.  Unsafe when the source
--   array can be modified later.
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: Ix i => STBitArray s i -> ST s (BitArray i)
unsafeFreeze (STB a) = unsafeIOToST (IO.unsafeFreeze a)

-- | Convert an array from immutable form.
{-# INLINE thaw #-}
thaw :: Ix i => BitArray i -> ST s (STBitArray s i)
thaw a = STB `fmap` unsafeIOToST (IO.thaw a)

-- | Convert an array from immutable form.  Unsafe to modify the result
--   unless the source array is never used later.
{-# INLINE unsafeThaw #-}
unsafeThaw :: Ix i => BitArray i -> ST s (STBitArray s i)
unsafeThaw a = STB `fmap` unsafeIOToST (IO.unsafeThaw a)

-- | Copy an array.
{-# INLINE copy #-}
copy :: Ix i => STBitArray s i -> ST s (STBitArray s i)
copy (STB a) = STB `fmap` unsafeIOToST (IO.copy a)

-- | Fill an array with a uniform value.
{-# INLINE fill #-}
fill :: Ix i => STBitArray s i -> Bool -> ST s ()
fill (STB a) b = unsafeIOToST (IO.fill a b)

-- | Short-circuit bitwise reduction: True when any bit is True.
{-# INLINE or #-}
or :: Ix i => STBitArray s i -> ST s Bool
or (STB a) = unsafeIOToST (IO.or a)

-- | Short-circuit bitwise reduction: False when any bit is False.
{-# INLINE and #-}
and :: Ix i => STBitArray s i -> ST s Bool
and (STB a) = unsafeIOToST (IO.and a)

-- | Short-circuit bitwise reduction: 'Nothing' when any bits differ,
--   'Just' when all bits are the same.
{-# INLINE isUniform #-}
isUniform :: Ix i => STBitArray s i -> ST s (Maybe Bool)
isUniform (STB a) = unsafeIOToST (IO.isUniform a)

-- | Look up index of first matching bit.
--
--   Note that the index type is limited to Int because there
--   is no 'unindex' method in the 'Ix' class.
{-# INLINE elemIndex #-}
elemIndex :: Bool -> STBitArray s Int -> ST s (Maybe Int)
elemIndex b (STB a) = unsafeIOToST (IO.elemIndex b a)

-- | Bitwise reduction with an associative commutative boolean operator.
--   Implementation lifts from 'Bool' to 'Bits' and folds large chunks
--   at a time.  Each bit is used as a source exactly once.
{-# INLINE fold #-}
fold :: Ix i => (Bool -> Bool -> Bool) {- ^ operator -} -> STBitArray s i -> ST s (Maybe Bool)
fold f (STB a) = unsafeIOToST (IO.fold f a)

-- | Bitwise map.  Implementation lifts from 'Bool' to 'Bits' and maps
--   large chunks at a time.
{-# INLINE map #-}
map :: Ix i => (Bool -> Bool) -> STBitArray s i -> ST s (STBitArray s i)
map f (STB a) = STB `fmap` unsafeIOToST (IO.map f a)

-- | Bitwise zipWith.  Implementation lifts from 'Bool' to 'Bits' and
--   combines large chunks at a time.
--
--   The bounds of the source arrays must be identical.
{-# INLINE zipWith #-}
zipWith :: Ix i => (Bool -> Bool -> Bool) -> STBitArray s i -> STBitArray s i -> ST s (STBitArray s i)
zipWith f (STB a) (STB b) = STB `fmap` unsafeIOToST (IO.zipWith f a b)

-- | Count set bits.
{-# INLINE popCount #-}
popCount :: Ix i => STBitArray s i -> ST s Int
popCount (STB a) = unsafeIOToST (IO.popCount a)
