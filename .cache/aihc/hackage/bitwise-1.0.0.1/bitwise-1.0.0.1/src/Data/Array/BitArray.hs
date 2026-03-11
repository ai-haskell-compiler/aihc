{-|

Module      :  Data.Array.BitArray
Copyright   :  (c) Claude Heiland-Allen 2012
License     :  BSD3

Maintainer  :  claude@mathr.co.uk
Stability   :  unstable
Portability :  portable

Immutable unboxed packed bit arrays using bitwise operations to
manipulate large chunks at a time much more quickly than individually
unpacking and repacking bits would allow.

-}
-- almost all is implemented with runST and the ST-based implementation
module Data.Array.BitArray
  ( BitArray()
  -- * IArray-like interface.
  , bounds
  , array
  , listArray
  , accumArray
  , (!)
  , indices
  , elems
  , assocs
  , (//)
  , accum
  , amap
  , ixmap
  -- * Constant arrays.
  , fill
  , false
  , true
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
  -- * Bounds-checked indexing.
  , (!?)
  -- * Unsafe.
  , (!!!)
  ) where

import Prelude hiding (and, or, map, zipWith)
import qualified Prelude as P

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Ix (Ix, range, inRange)

import Data.Array.BitArray.Internal (BitArray)
import qualified Data.Array.BitArray.ST as ST

-- | The bounds of an array.
{-# INLINE bounds #-}
bounds :: Ix i => BitArray i -> (i, i)
bounds a = runST (ST.getBounds =<< ST.unsafeThaw a)

-- | Create an array from a list of (index, element) pairs.
{-# INLINE array #-}
array :: Ix i => (i, i) {- ^ bounds -} -> [(i, Bool)] {- ^ assocs -} -> BitArray i
array bs ies = false bs // ies

-- | Create an array from a list of elements.
{-# INLINE listArray #-}
listArray :: Ix i => (i, i) {- ^ bounds -} -> [Bool] {- ^ elems -} -> BitArray i
listArray bs es = runST (ST.unsafeFreeze =<< ST.newListArray bs es)

-- | Create an array by accumulating a list of (index, operand) pairs
--   from a default seed with an operation.
{-# INLINE accumArray #-}
accumArray :: Ix i => (Bool -> a -> Bool) {- ^ operation -} -> Bool {- ^ default -} -> (i, i) {- ^ bounds -} -> [(i, a)] {- ^ assocs -} -> BitArray i
accumArray f d bs = accum f (fill bs d)

-- | Bit array indexing.
{-# INLINE (!) #-}
(!) :: Ix i => BitArray i -> i -> Bool
a ! i = runST (do
  a' <- ST.unsafeThaw a
  ST.readArray a' i)

-- | Bit array indexing without bounds checking.  Unsafe.
{-# INLINE (!!!) #-}
(!!!) :: Ix i => BitArray i -> i -> Bool
a !!! i = runST (do
  a' <- ST.unsafeThaw a
  ST.unsafeReadArray a' i)

-- | A list of all the valid indices for this array.
{-# INLINE indices #-}
indices :: Ix i => BitArray i -> [i]
indices = range . bounds

-- | A list of the elements in this array.
{-# INLINE elems #-}
elems :: Ix i => BitArray i -> [Bool]
elems a = runST (ST.unsafeGetElems =<< ST.unsafeThaw a)
  -- P.map (a !!!) (indices a) -- very slow!

-- | A list of the (index, element) pairs in this array.
{-# INLINE assocs #-}
assocs :: Ix i => BitArray i -> [(i, Bool)]
assocs ba = P.map (\i -> (i, ba ! i)) (indices ba)

-- | A new array with updated values at the supplied indices.
{-# INLINE (//) #-}
(//) :: Ix i => BitArray i -> [(i, Bool)] {- ^ new assocs -} -> BitArray i
ba // ies = accum (\_ a -> a) ba ies

-- | Accumulate with an operation and a list of (index, operand).
{-# INLINE accum #-}
accum :: Ix i => (Bool -> a -> Bool) {- ^ operation -} -> BitArray i {- ^ source -} -> [(i, a)] {- ^ assocs -} -> BitArray i
accum f a ies = runST (do
  a' <- ST.thaw a
  forM_ ies $ \(i, x) -> do
    b <- ST.readArray a' i
    ST.writeArray a' i (f b x)
  ST.unsafeFreeze a')

-- | Alias for 'map'.
{-# INLINE amap #-}
amap :: Ix i => (Bool -> Bool) -> BitArray i -> BitArray i
amap = map

-- | Create a new array by mapping indices into a source array..
{-# INLINE ixmap #-}
ixmap :: (Ix i, Ix j) => (i, i) {- ^ new bounds -} -> (i -> j) {- ^ index transformation -} -> BitArray j {- ^ source array -} -> BitArray i
ixmap bs h ba = array bs (P.map (\i -> (i, ba ! h i)) (range bs))

-- | A uniform array of bits.
{-# INLINE fill #-}
fill :: Ix i => (i, i) {- ^ bounds -} -> Bool -> BitArray i
fill bs b = runST (ST.unsafeFreeze =<< ST.newArray bs b)

-- | A uniform array of 'False'.
{-# INLINE false #-}
false :: Ix i => (i, i) {- ^ bounds -} -> BitArray i
false bs = fill bs False

-- | A uniform array of 'True'.
{-# INLINE true #-}
true :: Ix i => (i, i) {- ^ bounds -} -> BitArray i
true bs = fill bs True

-- | Bounds checking combined with array indexing.
{-# INLINE (!?) #-}
(!?) :: Ix i => BitArray i -> i -> Maybe Bool
b !? i
  | inRange (bounds b) i = Just (b ! i)
  | otherwise = Nothing

-- | Short-circuit bitwise reduction: True if any bit is True.
{-# INLINE or #-}
or :: Ix i => BitArray i -> Bool
or a = runST (ST.or =<< ST.unsafeThaw a)

-- | Short-circuit bitwise reduction: False if any bit is False.
{-# INLINE and #-}
and :: Ix i => BitArray i -> Bool
and a = runST (ST.and =<< ST.unsafeThaw a)

-- | Short-circuit bitwise reduction: Nothing if any bits differ.
{-# INLINE isUniform #-}
isUniform :: Ix i => BitArray i -> Maybe Bool
isUniform a = runST (ST.isUniform =<< ST.unsafeThaw a)

-- | Look up index of first matching bit.
--
--   Note that the index type is limited to Int because there
--   is no 'unindex' method in the 'Ix' class.
{-# INLINE elemIndex #-}
elemIndex :: Bool -> BitArray Int -> Maybe Int
elemIndex b a = runST (ST.elemIndex b =<< ST.unsafeThaw a)

-- | Bitwise reduction with an associative commutative boolean operator.
--   Implementation lifts from 'Bool' to 'Bits' and folds large chunks
--   at a time.  Each bit is used as a source exactly once.
{-# INLINE fold #-}
fold :: Ix i => (Bool -> Bool -> Bool) -> BitArray i -> Maybe Bool
fold f a = runST (ST.fold f =<< ST.unsafeThaw a)

-- | Bitwise map.  Implementation lifts from 'Bool' to 'Bits' and maps
--   large chunks at a time.
{-# INLINE map #-}
map :: Ix i => (Bool -> Bool) -> BitArray i -> BitArray i
map f a = runST (ST.unsafeFreeze =<< ST.map f =<< ST.unsafeThaw a)

-- | Bitwise zipWith.  Implementation lifts from 'Bool' to 'Bits' and
--   combines large chunks at a time.
--
--   The bounds of the source arrays must be identical.
{-# INLINE zipWith #-}
zipWith :: Ix i => (Bool -> Bool -> Bool) -> BitArray i -> BitArray i -> BitArray i
zipWith f a b
  | bounds a == bounds b = runST (do
      a' <- ST.unsafeThaw a
      b' <- ST.unsafeThaw b
      ST.unsafeFreeze =<< ST.zipWith f a' b')
  | otherwise = error "zipWith bounds mismatch"

-- | Count set bits.
{-# INLINE popCount #-}
popCount :: Ix i => BitArray i -> Int
popCount a = runST (ST.popCount =<< ST.unsafeThaw a)
