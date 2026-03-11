{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}

-- |
-- GHC contains three general classes of value types:
--
--   1. Unboxed types: values are machine values made up of fixed numbers of bytes.
--      These include types like @Int#@, @Char#@ and @Addr#@.
--   2. Unlifted types: values are pointers, but strictly evaluated. These include
--      types like @MutVar# s a@, @Array# a@, and @MVar# s a@.
--   3. Lifted types: values are pointers, lazily evaluated.
--
-- Certain lifted types are really just thin wrappers around unboxed types (we can call
-- these category 3a) or unlifted pointer types (we can call these category 3b)
-- Category 3a includes `Int`, `Char`, and `Ptr a`, while category 3b includes
-- @IORef a@, @Data.Primitive.Array.Array a@, and @MVar a@.
--
-- Types in category 3a can be stored efficiently in a @Data.Primitive.PrimArray.PrimArray@,
-- removing and applying wrappers as required. This module provides the same facility for
-- types in category 3b.
module Data.Primitive.Unlifted.Array
  ( -- * Types
    A.UnliftedArray_(..)
  , A.UnliftedArray
  , A.MutableUnliftedArray_(..)
  , A.MutableUnliftedArray
    -- * Operations
  , newUnliftedArray
  , unsafeNewUnliftedArray
  , A.sizeofUnliftedArray
  , A.sizeofMutableUnliftedArray
  , A.sameMutableUnliftedArray
  , writeUnliftedArray
  , readUnliftedArray
  , A.indexUnliftedArray
  , unsafeFreezeUnliftedArray
  , freezeUnliftedArray
  , thawUnliftedArray
  , unsafeThawUnliftedArray
  , setUnliftedArray
  , copyUnliftedArray
  , copyMutableUnliftedArray
  , A.cloneUnliftedArray
  , cloneMutableUnliftedArray
  , A.emptyUnliftedArray
  , A.singletonUnliftedArray
  , A.runUnliftedArray
  , A.dupableRunUnliftedArray
    -- * List Conversion
  , A.unliftedArrayToList
  , A.unliftedArrayFromList
  , A.unliftedArrayFromListN
    -- * Folding
  , A.foldrUnliftedArray
  , A.foldrUnliftedArray'
  , A.foldlUnliftedArray
  , A.foldlUnliftedArray'
  , A.foldlUnliftedArrayM'
    -- * Traversals
  , A.traverseUnliftedArray_
  , A.itraverseUnliftedArray_
    -- * Mapping
  , A.mapUnliftedArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState,stToPrim)
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import qualified Data.Primitive.Unlifted.Array.ST as A
import Data.Primitive.Unlifted.Array.ST (UnliftedArray, MutableUnliftedArray)

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents.
newUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => Int -- ^ size
  -> a -- ^ initial value
  -> m (MutableUnliftedArray (PrimState m) a)
newUnliftedArray len v = stToPrim $ A.newUnliftedArray len v
{-# inline newUnliftedArray #-}

setUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m ()
{-# inline setUnliftedArray #-}
setUnliftedArray mua v off len = stToPrim $ A.setUnliftedArray mua v off len

writeUnliftedArray :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a
  -> Int
  -> a
  -> m ()
{-# inline writeUnliftedArray #-}
writeUnliftedArray mary ix a = stToPrim $ A.writeUnliftedArray mary ix a

readUnliftedArray :: (PrimMonad m, PrimUnlifted a)
  => MutableUnliftedArray (PrimState m) a
  -> Int
  -> m a
{-# inline readUnliftedArray #-}
readUnliftedArray mary ix = stToPrim $ A.readUnliftedArray mary ix

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a
  -> m (UnliftedArray a)
unsafeFreezeUnliftedArray mary = stToPrim $ A.unsafeFreezeUnliftedArray mary
{-# inline unsafeFreezeUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
  :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> UnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copyUnliftedArray #-}
copyUnliftedArray dst doff src soff ln = stToPrim $ A.copyUnliftedArray dst doff src soff ln

-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copyMutableUnliftedArray #-}
copyMutableUnliftedArray dst doff src soff ln = stToPrim $ A.copyMutableUnliftedArray dst doff src soff ln

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
  :: PrimMonad m
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (UnliftedArray a)
freezeUnliftedArray mary off len = stToPrim $ A.freezeUnliftedArray mary off len
{-# inline freezeUnliftedArray #-}

-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
  :: PrimMonad m
  => UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline thawUnliftedArray #-}
thawUnliftedArray ary off len = stToPrim $ A.thawUnliftedArray ary off len

-- | Thaws an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This does not make a copy.
unsafeThawUnliftedArray
  :: PrimMonad m
  => UnliftedArray a -- ^ source
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline unsafeThawUnliftedArray #-}
unsafeThawUnliftedArray ary = stToPrim $ A.unsafeThawUnliftedArray ary

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'UnliftedArray' a@ to the element type.
unsafeNewUnliftedArray
  :: PrimMonad m
  => Int -- ^ size
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline unsafeNewUnliftedArray #-}
unsafeNewUnliftedArray len = stToPrim $ A.unsafeNewUnliftedArray len

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
  :: (PrimMonad m)
  => MutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (MutableUnliftedArray (PrimState m) a)
{-# inline cloneMutableUnliftedArray #-}
cloneMutableUnliftedArray mary off len = stToPrim $ A.cloneMutableUnliftedArray mary off len
