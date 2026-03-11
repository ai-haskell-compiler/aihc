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
module Data.Primitive.Unlifted.SmallArray
  ( -- * Types
    A.SmallUnliftedArray_(..)
  , A.SmallUnliftedArray
  , A.SmallMutableUnliftedArray_(..)
  , A.SmallMutableUnliftedArray
    -- * Operations
  , newSmallUnliftedArray
  , unsafeNewSmallUnliftedArray
  , A.sizeofSmallUnliftedArray
  , getSizeofSmallMutableUnliftedArray
  , A.sameSmallMutableUnliftedArray
  , shrinkSmallMutableUnliftedArray
  , writeSmallUnliftedArray
  , readSmallUnliftedArray
  , A.indexSmallUnliftedArray
  , unsafeFreezeSmallUnliftedArray
  , freezeSmallUnliftedArray
  , thawSmallUnliftedArray
  , unsafeThawSmallUnliftedArray
  , setSmallUnliftedArray
  , copySmallUnliftedArray
  , copySmallMutableUnliftedArray
  , A.cloneSmallUnliftedArray
  , cloneSmallMutableUnliftedArray
  , A.emptySmallUnliftedArray
  , A.singletonSmallUnliftedArray
  , A.runSmallUnliftedArray
  , A.dupableRunSmallUnliftedArray
    -- * List Conversion
  , A.smallUnliftedArrayToList
  , A.smallUnliftedArrayFromList
  , A.smallUnliftedArrayFromListN
    -- * Folding
  , A.foldrSmallUnliftedArray
  , A.foldrSmallUnliftedArray'
  , A.foldlSmallUnliftedArray
  , A.foldlSmallUnliftedArray'
  , A.foldlSmallUnliftedArrayM'
    -- * Traversals
  , A.traverseSmallUnliftedArray_
  , A.itraverseSmallUnliftedArray_
    -- * Mapping
  , A.mapSmallUnliftedArray
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState,stToPrim)
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import qualified Data.Primitive.Unlifted.SmallArray.ST as A
import Data.Primitive.Unlifted.SmallArray.ST (SmallUnliftedArray, SmallMutableUnliftedArray)

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents.
newSmallUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => Int -- ^ size
  -> a -- ^ initial value
  -> m (SmallMutableUnliftedArray (PrimState m) a)
newSmallUnliftedArray len v = stToPrim $ A.newSmallUnliftedArray len v
{-# inline newSmallUnliftedArray #-}

setSmallUnliftedArray
  :: (PrimMonad m, PrimUnlifted a)
  => SmallMutableUnliftedArray (PrimState m) a -- ^ destination
  -> a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m ()
{-# inline setSmallUnliftedArray #-}
setSmallUnliftedArray mua v off len = stToPrim $ A.setSmallUnliftedArray mua v off len

shrinkSmallMutableUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a
  -> Int
  -> m ()
shrinkSmallMutableUnliftedArray mary sz = stToPrim $ A.shrinkSmallMutableUnliftedArray mary sz
{-# inline shrinkSmallMutableUnliftedArray #-}

writeSmallUnliftedArray :: (PrimMonad m, PrimUnlifted a)
  => SmallMutableUnliftedArray (PrimState m) a
  -> Int
  -> a
  -> m ()
{-# inline writeSmallUnliftedArray #-}
writeSmallUnliftedArray mary ix a = stToPrim $ A.writeSmallUnliftedArray mary ix a

readSmallUnliftedArray :: (PrimMonad m, PrimUnlifted a)
  => SmallMutableUnliftedArray (PrimState m) a
  -> Int
  -> m a
{-# inline readSmallUnliftedArray #-}
readSmallUnliftedArray mary ix = stToPrim $ A.readSmallUnliftedArray mary ix

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeSmallUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a
  -> m (SmallUnliftedArray a)
unsafeFreezeSmallUnliftedArray mary = stToPrim $ A.unsafeFreezeSmallUnliftedArray mary
{-# inline unsafeFreezeSmallUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copySmallUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> SmallUnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copySmallUnliftedArray #-}
copySmallUnliftedArray dst doff src soff ln = stToPrim $ A.copySmallUnliftedArray dst doff src soff ln

-- | Copies the contents of one mutable array into another.
copySmallMutableUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a -- ^ destination
  -> Int -- ^ offset into destination
  -> SmallMutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> m ()
{-# inline copySmallMutableUnliftedArray #-}
copySmallMutableUnliftedArray dst doff src soff ln = stToPrim $ A.copySmallMutableUnliftedArray dst doff src soff ln

-- | Freezes a portion of a 'SmallMutableUnliftedArray', yielding a 'SmallUnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeSmallUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (SmallUnliftedArray a)
freezeSmallUnliftedArray mary off len = stToPrim $ A.freezeSmallUnliftedArray mary off len
{-# inline freezeSmallUnliftedArray #-}

-- | Thaws a portion of a 'SmallUnliftedArray', yielding a 'SmallMutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawSmallUnliftedArray
  :: PrimMonad m
  => SmallUnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (SmallMutableUnliftedArray (PrimState m) a)
{-# inline thawSmallUnliftedArray #-}
thawSmallUnliftedArray ary off len = stToPrim $ A.thawSmallUnliftedArray ary off len

-- | Thaw a 'SmallUnliftedArray', yielding a 'SmallMutableUnliftedArray'.
-- This does not make a copy.
unsafeThawSmallUnliftedArray
  :: PrimMonad m
  => SmallUnliftedArray a -- ^ source
  -> m (SmallMutableUnliftedArray (PrimState m) a)
{-# inline unsafeThawSmallUnliftedArray #-}
unsafeThawSmallUnliftedArray ary = stToPrim $ A.unsafeThawSmallUnliftedArray ary

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'UnliftedArray' a@ to the element type.
unsafeNewSmallUnliftedArray
  :: PrimMonad m
  => Int -- ^ size
  -> m (SmallMutableUnliftedArray (PrimState m) a)
{-# inline unsafeNewSmallUnliftedArray #-}
unsafeNewSmallUnliftedArray len = stToPrim $ A.unsafeNewSmallUnliftedArray len

-- | Yields the length of a 'MutableUnliftedArray'.
getSizeofSmallMutableUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a
  -> m Int
getSizeofSmallMutableUnliftedArray a = stToPrim $ A.getSizeofSmallMutableUnliftedArray a

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneSmallMutableUnliftedArray
  :: PrimMonad m
  => SmallMutableUnliftedArray (PrimState m) a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> m (SmallMutableUnliftedArray (PrimState m) a)
{-# inline cloneSmallMutableUnliftedArray #-}
cloneSmallMutableUnliftedArray mary off len = stToPrim $ A.cloneSmallMutableUnliftedArray mary off len
