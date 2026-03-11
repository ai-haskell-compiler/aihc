{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}

-- |
-- A version of the 'Data.Primitive.Unlifted.SmallArray' interface
-- specialized to 'ST'. This is intended primarily so library
-- developers can easily check whether the basic operations are
-- unboxed properly, but its more constrained type signatures
-- also offer somewhat better type inference where applicable.
module Data.Primitive.Unlifted.SmallArray.ST
  ( -- * Types
    SmallUnliftedArray_(..)
  , SmallUnliftedArray
  , SmallMutableUnliftedArray_(..)
  , SmallMutableUnliftedArray
    -- * Operations
  , newSmallUnliftedArray
  , unsafeNewSmallUnliftedArray
  , sizeofSmallUnliftedArray
  , getSizeofSmallMutableUnliftedArray
  , sameSmallMutableUnliftedArray
  , shrinkSmallMutableUnliftedArray
  , writeSmallUnliftedArray
  , readSmallUnliftedArray
  , indexSmallUnliftedArray
  , unsafeFreezeSmallUnliftedArray
  , freezeSmallUnliftedArray
  , thawSmallUnliftedArray
  , unsafeThawSmallUnliftedArray
  , setSmallUnliftedArray
  , copySmallUnliftedArray
  , copySmallMutableUnliftedArray
  , cloneSmallUnliftedArray
  , cloneSmallMutableUnliftedArray
  , emptySmallUnliftedArray
  , singletonSmallUnliftedArray
  , runSmallUnliftedArray
  , dupableRunSmallUnliftedArray
    -- * List Conversion
  , smallUnliftedArrayToList
  , smallUnliftedArrayFromList
  , smallUnliftedArrayFromListN
    -- * Folding
  , foldrSmallUnliftedArray
  , foldrSmallUnliftedArray'
  , foldlSmallUnliftedArray
  , foldlSmallUnliftedArray'
  , foldlSmallUnliftedArrayM'
    -- * Traversals
  , traverseSmallUnliftedArray_
  , itraverseSmallUnliftedArray_
    -- * Mapping
  , mapSmallUnliftedArray
  ) where

import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.SmallArray.Primops
import GHC.Exts (Int(I#),State#)
import GHC.ST (ST (..))

import qualified Data.List as L
import qualified GHC.Exts as Exts

-- | Using a specialized copy of primitive_ here makes the Core a little
-- easier to read by eliminating unnecessary PrimState coercions.
primitive_ :: (State# s -> State# s) -> ST s ()
{-# INLINE primitive_ #-}
primitive_ m = ST (\s -> (# m s, () #))

-- | A @SmallUnliftedArray_ a unlifted_a@ represents an array of values of a
-- lifted type @a@ that wrap values of an unlifted type @unlifted_a@.
-- It is expected that @unlifted_a ~ Unlifted a@, but imposing that constraint
-- here would force the type roles to @nominal@, which is often undesirable
-- when arrays are used as components of larger datatypes.
data SmallUnliftedArray_ unlifted_a a 
  = SmallUnliftedArray (SmallUnliftedArray# unlifted_a)
type role SmallUnliftedArray_ representational phantom 

-- | A type synonym for a 'SmallUnliftedArray_' containing lifted values of
-- a particular type. As a general rule, this type synonym should not be used in
-- class instances—use 'SmallUnliftedArray_' with an equality constraint instead.
-- It also should not be used when defining newtypes or datatypes, unless those
-- will have restrictive type roles regardless—use 'SmallUnliftedArray_' instead.
type SmallUnliftedArray a = SmallUnliftedArray_ (Unlifted a) a 

data SmallMutableUnliftedArray_ unlifted_a s a
  = SmallMutableUnliftedArray (SmallMutableUnliftedArray# s unlifted_a)
type role SmallMutableUnliftedArray_ representational nominal phantom 

type SmallMutableUnliftedArray s a = SmallMutableUnliftedArray_ (Unlifted a) s a 

instance unlifted_a ~ Unlifted a => PrimUnlifted (SmallUnliftedArray_ unlifted_a a) where
  type Unlifted (SmallUnliftedArray_ unlifted_a _) = SmallUnliftedArray# unlifted_a
  toUnlifted# (SmallUnliftedArray a) = a
  fromUnlifted# x = SmallUnliftedArray x

instance unlifted_a ~ Unlifted a => PrimUnlifted (SmallMutableUnliftedArray_ unlifted_a s a) where
  type Unlifted (SmallMutableUnliftedArray_ unlifted_a s _) = SmallMutableUnliftedArray# s unlifted_a 
  toUnlifted# (SmallMutableUnliftedArray a) = a
  fromUnlifted# x = SmallMutableUnliftedArray x

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents.
newSmallUnliftedArray
  :: PrimUnlifted a
  => Int -- ^ size
  -> a -- ^ initial value
  -> ST s (SmallMutableUnliftedArray s a)
newSmallUnliftedArray (I# len) v = ST $ \s -> case newSmallUnliftedArray# len (toUnlifted# v) s of
  (# s', ma #) -> (# s', SmallMutableUnliftedArray ma #)
{-# inline newSmallUnliftedArray #-}

setSmallUnliftedArray
  :: PrimUnlifted a
  => SmallMutableUnliftedArray s a -- ^ destination
  -> a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s ()
{-# inline setSmallUnliftedArray #-}
setSmallUnliftedArray mua v off len = loop (len + off - 1)
 where
 loop i
   | i < off = pure ()
   | otherwise = writeSmallUnliftedArray mua i v *> loop (i-1)

-- | Yields the length of an 'UnliftedArray'.
sizeofSmallUnliftedArray :: SmallUnliftedArray_ unlifted_e e -> Int
{-# inline sizeofSmallUnliftedArray #-}
sizeofSmallUnliftedArray (SmallUnliftedArray ar) = I# (sizeofSmallUnliftedArray# ar)

-- | Yields the length of a 'MutableUnliftedArray'.
getSizeofSmallMutableUnliftedArray :: SmallMutableUnliftedArray s e -> ST s Int
{-# inline getSizeofSmallMutableUnliftedArray #-}
getSizeofSmallMutableUnliftedArray (SmallMutableUnliftedArray maa#)
  = ST (\s -> case getSizeofSmallMutableUnliftedArray# maa# s of
      (# s', sz #) -> (# s', I# sz #))

writeSmallUnliftedArray :: PrimUnlifted a
  => SmallMutableUnliftedArray s a
  -> Int
  -> a
  -> ST s ()
{-# inline writeSmallUnliftedArray #-}
writeSmallUnliftedArray (SmallMutableUnliftedArray arr) (I# ix) a =
  primitive_ (writeSmallUnliftedArray# arr ix (toUnlifted# a))

readSmallUnliftedArray :: PrimUnlifted a
  => SmallMutableUnliftedArray s a
  -> Int
  -> ST s a
{-# inline readSmallUnliftedArray #-}
readSmallUnliftedArray (SmallMutableUnliftedArray arr) (I# ix) =
  ST $ \s -> case readSmallUnliftedArray# arr ix s of
    (# s', a #) -> (# s', fromUnlifted# a #)

indexSmallUnliftedArray :: PrimUnlifted a
  => SmallUnliftedArray a
  -> Int
  -> a
{-# inline indexSmallUnliftedArray #-}
indexSmallUnliftedArray (SmallUnliftedArray arr) (I# ix) =
  fromUnlifted# (indexSmallUnliftedArray# arr ix)

-- | Freezes a 'SmallMutableUnliftedArray', yielding a 'SmallUnliftedArray'.
-- This simply marks the array as frozen in place, so it should only be used
-- when no further modifications to the mutable array will be performed.
unsafeFreezeSmallUnliftedArray
  :: SmallMutableUnliftedArray s a
  -> ST s (SmallUnliftedArray a)
unsafeFreezeSmallUnliftedArray (SmallMutableUnliftedArray maa#)
  = ST $ \s -> case unsafeFreezeSmallUnliftedArray# maa# s of
      (# s', aa# #) -> (# s', SmallUnliftedArray aa# #)
{-# inline unsafeFreezeSmallUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameSmallMutableUnliftedArray
  :: SmallMutableUnliftedArray_ unlifted_a s a
  -> SmallMutableUnliftedArray_ unlifted_a s a
  -> Bool
sameSmallMutableUnliftedArray (SmallMutableUnliftedArray maa1#) (SmallMutableUnliftedArray maa2#)
  = Exts.isTrue# (sameSmallMutableUnliftedArray# maa1# maa2#)
{-# inline sameSmallMutableUnliftedArray #-}

-- | Shrink a mutable array to the specified size. The new size argument must be less than or
-- equal to the current size.
shrinkSmallMutableUnliftedArray :: SmallMutableUnliftedArray s a -> Int -> ST s ()
shrinkSmallMutableUnliftedArray (SmallMutableUnliftedArray mary) (I# sz)
  = primitive_ $ shrinkSmallMutableUnliftedArray# mary sz
{-# inline shrinkSmallMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copySmallUnliftedArray
  :: SmallMutableUnliftedArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> SmallUnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copySmallUnliftedArray #-}
copySmallUnliftedArray
  (SmallMutableUnliftedArray dst) (I# doff)
  (SmallUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copySmallUnliftedArray# src soff dst doff ln

-- | Copies the contents of one mutable array into another.
copySmallMutableUnliftedArray
  :: SmallMutableUnliftedArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> SmallMutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copySmallMutableUnliftedArray #-}
copySmallMutableUnliftedArray
  (SmallMutableUnliftedArray dst) (I# doff)
  (SmallMutableUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copySmallMutableUnliftedArray# src soff dst doff ln

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeSmallUnliftedArray
  :: SmallMutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (SmallUnliftedArray a)
freezeSmallUnliftedArray (SmallMutableUnliftedArray mary) (I# off) (I# len) =
    ST $ \s -> case freezeSmallUnliftedArray# mary off len s of
      (# s', ary #) -> (# s', SmallUnliftedArray ary #)
{-# inline freezeSmallUnliftedArray #-}

-- | Thaws a portion of a 'SmallUnliftedArray', yielding a 'SmallMutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawSmallUnliftedArray
  :: SmallUnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (SmallMutableUnliftedArray s a)
{-# inline thawSmallUnliftedArray #-}
thawSmallUnliftedArray (SmallUnliftedArray ary) (I# off) (I# len) =
    ST $ \s -> case thawSmallUnliftedArray# ary off len s of
      (# s', mary #) -> (# s', SmallMutableUnliftedArray mary #)

-- | Thaws a 'SmallUnliftedArray', yielding a 'SmallMutableUnliftedArray'.
-- This does not make a copy.
unsafeThawSmallUnliftedArray
  :: SmallUnliftedArray a -- ^ source
  -> ST s (SmallMutableUnliftedArray s a)
{-# inline unsafeThawSmallUnliftedArray #-}
unsafeThawSmallUnliftedArray (SmallUnliftedArray ary) =
    ST $ \s -> case unsafeThawSmallUnliftedArray# ary s of
      (# s', mary #) -> (# s', SmallMutableUnliftedArray mary #)

-- | Execute a stateful computation and freeze the resulting array.
runSmallUnliftedArray
  :: (forall s. ST s (SmallMutableUnliftedArray s a))
  -> SmallUnliftedArray a
{-# INLINE runSmallUnliftedArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- runUnliftedArray m = runST $ noDuplicate >> m >>= unsafeFreezeUnliftedArray
runSmallUnliftedArray m = SmallUnliftedArray (runSmallUnliftedArray# m)

runSmallUnliftedArray#
  :: (forall s. ST s (SmallMutableUnliftedArray s a))
  -> SmallUnliftedArray# (Unlifted a)
runSmallUnliftedArray# m = case Exts.runRW# $ \s0 ->
  case Exts.noDuplicate# s0 of { s ->
  case unST m s of { (# s', SmallMutableUnliftedArray mary# #) ->
  unsafeFreezeSmallUnliftedArray# mary# s'}} of (# _, ary# #) -> ary#
{-# INLINE runSmallUnliftedArray# #-}

-- | Execute a stateful computation and freeze the resulting array.
-- It is possible, but unlikely, that the computation will be run
-- multiple times in multiple threads.
dupableRunSmallUnliftedArray
  :: (forall s. ST s (SmallMutableUnliftedArray s a))
  -> SmallUnliftedArray a
{-# INLINE dupableRunSmallUnliftedArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- dupableRunUnliftedArray m = runST $ m >>= unsafeFreezeUnliftedArray
dupableRunSmallUnliftedArray m = SmallUnliftedArray (dupableRunSmallUnliftedArray# m)

dupableRunSmallUnliftedArray#
  :: (forall s. ST s (SmallMutableUnliftedArray s a))
  -> SmallUnliftedArray# (Unlifted a)
dupableRunSmallUnliftedArray# m = case Exts.runRW# $ \s ->
  case unST m s of { (# s', SmallMutableUnliftedArray mary# #) ->
  unsafeFreezeSmallUnliftedArray# mary# s'} of (# _, ary# #) -> ary#
{-# INLINE dupableRunSmallUnliftedArray# #-}

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

unsafeCreateSmallUnliftedArray
  :: Int
  -> (forall s. SmallMutableUnliftedArray s a -> ST s ())
  -> SmallUnliftedArray a
unsafeCreateSmallUnliftedArray !n f = runSmallUnliftedArray $ do
  mary <- unsafeNewSmallUnliftedArray n
  f mary
  pure mary

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'UnliftedArray' a@ to the element type.
unsafeNewSmallUnliftedArray
  :: Int -- ^ size
  -> ST s (SmallMutableUnliftedArray s a)
{-# inline unsafeNewSmallUnliftedArray #-}
unsafeNewSmallUnliftedArray (I# i) = ST $ \s -> case unsafeNewSmallUnliftedArray# i s of
  (# s', ma #) -> (# s', SmallMutableUnliftedArray ma #)


-- | Creates a copy of a portion of a 'SmallUnliftedArray'
cloneSmallUnliftedArray
  :: SmallUnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> SmallUnliftedArray a
{-# inline cloneSmallUnliftedArray #-}
cloneSmallUnliftedArray (SmallUnliftedArray ary) (I# off) (I# len)
  = SmallUnliftedArray (cloneSmallUnliftedArray# ary off len)

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneSmallMutableUnliftedArray
  :: SmallMutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (SmallMutableUnliftedArray s a)
{-# inline cloneSmallMutableUnliftedArray #-}
cloneSmallMutableUnliftedArray (SmallMutableUnliftedArray mary) (I# off) (I# len)
  = ST $ \s -> case cloneSmallMutableUnliftedArray# mary off len s of
      (# s', mary' #) -> (# s', SmallMutableUnliftedArray mary' #)

emptySmallUnliftedArray :: SmallUnliftedArray_ unlifted_a a
emptySmallUnliftedArray = SmallUnliftedArray (emptySmallUnliftedArray# (##))

singletonSmallUnliftedArray :: PrimUnlifted a => a -> SmallUnliftedArray a
{-# INLINE singletonSmallUnliftedArray #-}
singletonSmallUnliftedArray x = runSmallUnliftedArray $ newSmallUnliftedArray 1 x

concatSmallUnliftedArray :: SmallUnliftedArray a -> SmallUnliftedArray a -> SmallUnliftedArray a
{-# INLINE concatSmallUnliftedArray #-}
concatSmallUnliftedArray (SmallUnliftedArray a1) (SmallUnliftedArray a2)
  = SmallUnliftedArray (concatSmallUnliftedArray# a1 a2)

-- This junk is to make sure we unbox properly. Inlining this doesn't seem
-- likely to be much of a win ever, and could potentially lead to reboxing,
-- so we NOINLINE. It would be nice to find a prettier way to do this.
concatSmallUnliftedArray# :: SmallUnliftedArray# a -> SmallUnliftedArray# a -> SmallUnliftedArray# a
{-# NOINLINE concatSmallUnliftedArray# #-}
concatSmallUnliftedArray# a1 a2 =
  let !sza1 = sizeofSmallUnliftedArray# a1
  in
    if Exts.isTrue# (sza1 Exts.==# 0#)
    then a2
    else
      let !sza2 = sizeofSmallUnliftedArray# a2
      in
        if Exts.isTrue# (sza2 Exts.==# 0#)
        then a1
        else Exts.runRW# $ \s0 ->
          let
            finish s =
              case unsafeNewSmallUnliftedArray# (sza1 Exts.+# sza2) s of { (# s', ma #) ->
              case copySmallUnliftedArray# a1 0# ma 0# sza1 s' of { s'' ->
              case copySmallUnliftedArray# a2 0# ma sza1 sza2 s'' of { s''' ->
              case unsafeFreezeSmallUnliftedArray# ma s''' of
                (# _, ar #) -> ar}}}
            -- GHC wants to inline this, but I very much doubt it's worth the
            -- extra code, considering that it calls multiple out-of-line
            -- primops.
            {-# NOINLINE finish #-}
          in
            -- When the final array will be "small", we tolerate the possibility that
            -- it could be constructed multiple times in different threads. Currently,
            -- "small" means fewer than 1000 elements. This is a totally arbitrary
            -- cutoff that has not been tuned whatsoever.
            if Exts.isTrue# ((sza1 Exts.+# sza2) Exts.>=# 1000#)
            then finish (Exts.noDuplicate# s0)
            else finish s0

foldrSmallUnliftedArray :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> SmallUnliftedArray a -> b
{-# INLINE foldrSmallUnliftedArray #-}
foldrSmallUnliftedArray f z arr = go 0
  where
    !sz = sizeofSmallUnliftedArray arr
    go !i
      | sz > i = f (indexSmallUnliftedArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of an 'SmallUnliftedArray.
{-# INLINE foldrSmallUnliftedArray' #-}
foldrSmallUnliftedArray' :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> SmallUnliftedArray a -> b
foldrSmallUnliftedArray' f z0 arr = go (sizeofSmallUnliftedArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexSmallUnliftedArray arr i) acc)

-- | Lazy left-associated fold over the elements of an 'SmallUnliftedArray'.
{-# INLINE foldlSmallUnliftedArray #-}
foldlSmallUnliftedArray :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> SmallUnliftedArray a -> b
foldlSmallUnliftedArray f z arr = go (sizeofSmallUnliftedArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexSmallUnliftedArray arr i)

-- | Strict left-associated fold over the elements of an 'SmallUnliftedArray'.
{-# INLINE foldlSmallUnliftedArray' #-}
foldlSmallUnliftedArray' :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> SmallUnliftedArray a -> b
foldlSmallUnliftedArray' f z0 arr = go 0 z0
  where
    !sz = sizeofSmallUnliftedArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexSmallUnliftedArray arr i))
      | otherwise = acc

-- | Strict effectful left-associated fold over the elements of an 'SmallUnliftedArray'.
{-# INLINE foldlSmallUnliftedArrayM' #-}
foldlSmallUnliftedArrayM' :: (PrimUnlifted a, Monad m)
  => (b -> a -> m b) -> b -> SmallUnliftedArray a -> m b
foldlSmallUnliftedArrayM' f z0 arr = go 0 z0
  where
    !sz = sizeofSmallUnliftedArray arr
    go !i !acc
      | i < sz = f acc (indexSmallUnliftedArray arr i) >>= go (i + 1) 
      | otherwise = pure acc

-- | Effectfully traverse the elements of an 'SmallUnliftedArray', discarding
-- the resulting values.
{-# INLINE traverseSmallUnliftedArray_ #-}
traverseSmallUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (a -> m b) -> SmallUnliftedArray a -> m ()
traverseSmallUnliftedArray_ f arr = go 0
  where
    !sz = sizeofSmallUnliftedArray arr
    go !i
      | i < sz = f (indexSmallUnliftedArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Effectful indexed traversal of the elements of an 'SmallUnliftedArray',
-- discarding the resulting values.
{-# INLINE itraverseSmallUnliftedArray_ #-}
itraverseSmallUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (Int -> a -> m b) -> SmallUnliftedArray a -> m ()
itraverseSmallUnliftedArray_ f arr = go 0
  where
    !sz = sizeofSmallUnliftedArray arr
    go !i
      | i < sz = f i (indexSmallUnliftedArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Map over the elements of an 'SmallUnliftedArray'.
{-# INLINE mapSmallUnliftedArray #-}
mapSmallUnliftedArray :: (PrimUnlifted a, PrimUnlifted b)
  => (a -> b)
  -> SmallUnliftedArray a
  -> SmallUnliftedArray b
-- See Data.Primitive.Unlifted.Array.ST for discussion of the noDuplicate#
-- buried in this unsafeCreateSmallUnliftedArray.
mapSmallUnliftedArray f arr = unsafeCreateSmallUnliftedArray sz $ \marr -> do
  let go !ix = if ix < sz
        then do
          let b = f (indexSmallUnliftedArray arr ix)
          writeSmallUnliftedArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  where
  !sz = sizeofSmallUnliftedArray arr

-- | Convert the unlifted array to a list.
{-# INLINE smallUnliftedArrayToList #-}
smallUnliftedArrayToList :: PrimUnlifted a => SmallUnliftedArray a -> [a]
smallUnliftedArrayToList xs = Exts.build (\c n -> foldrSmallUnliftedArray c n xs)

smallUnliftedArrayFromList :: PrimUnlifted a => [a] -> SmallUnliftedArray a
smallUnliftedArrayFromList xs = smallUnliftedArrayFromListN (L.length xs) xs

smallUnliftedArrayFromListN :: forall a. PrimUnlifted a => Int -> [a] -> SmallUnliftedArray a
smallUnliftedArrayFromListN len vs = unsafeCreateSmallUnliftedArray len run where
  run :: forall s. SmallMutableUnliftedArray s a -> ST s ()
  run arr = do
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          -- The size check is mandatory since failure to initialize all elements
          -- introduces the possibility of a segfault happening when someone attempts
          -- to read the unitialized element. See the docs for unsafeNewSmallUnliftedArray.
          then return ()
          else die "unliftedArrayFromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            writeSmallUnliftedArray arr ix a
            go as (ix + 1)
          else die "unliftedArrayFromListN" "list length greater than specified size"
    go vs 0

instance (PrimUnlifted a, unlifted_a ~ Unlifted a)
  => Exts.IsList (SmallUnliftedArray_ unlifted_a a) where
  type Item (SmallUnliftedArray_ _ a) = a
  fromList = smallUnliftedArrayFromList
  fromListN = smallUnliftedArrayFromListN
  toList = smallUnliftedArrayToList

instance (PrimUnlifted a, unlifted_a ~ Unlifted a)
  => Semigroup (SmallUnliftedArray_ unlifted_a a) where
  (<>) = concatSmallUnliftedArray

instance (PrimUnlifted a, unlifted_a ~ Unlifted a) => Monoid (SmallUnliftedArray_ unlifted_a a) where
  mempty = emptySmallUnliftedArray

instance (Show a, PrimUnlifted a, unlifted_a ~ Unlifted a) => Show (SmallUnliftedArray_ unlifted_a a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofSmallUnliftedArray a) . showString " "
      . shows (smallUnliftedArrayToList a)

instance unlifted_a ~ Unlifted a => Eq (SmallMutableUnliftedArray_ unlifted_a s a) where
  (==) = sameSmallMutableUnliftedArray

instance (Eq a, PrimUnlifted a, unlifted_a ~ Unlifted a) => Eq (SmallUnliftedArray_ unlifted_a a) where
  aa1 == aa2 = sizeofSmallUnliftedArray aa1 == sizeofSmallUnliftedArray aa2
            && loop (sizeofSmallUnliftedArray aa1 - 1)
   where
   loop i
     | i < 0 = True
     | otherwise = indexSmallUnliftedArray aa1 i == indexSmallUnliftedArray aa2 i && loop (i-1)

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.Unlifted.SmallArray.ST." ++ fun ++ ": " ++ problem
