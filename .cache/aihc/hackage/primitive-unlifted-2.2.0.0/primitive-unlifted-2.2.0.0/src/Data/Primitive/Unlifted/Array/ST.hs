{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}

-- |
-- A version of the 'Data.Primitive.Unlifted.Array' interface
-- specialized to 'ST'. This is intended primarily so library
-- developers can easily check whether the basic operations are
-- unboxed properly, but its more constrained type signatures
-- also offer somewhat better type inference where applicable.
module Data.Primitive.Unlifted.Array.ST
  ( -- * Types
    UnliftedArray_(..)
  , UnliftedArray
  , MutableUnliftedArray_(..)
  , MutableUnliftedArray
    -- * Operations
  , newUnliftedArray
  , unsafeNewUnliftedArray
  , sizeofUnliftedArray
  , sizeofMutableUnliftedArray
  , sameMutableUnliftedArray
  , writeUnliftedArray
  , readUnliftedArray
  , indexUnliftedArray
  , unsafeFreezeUnliftedArray
  , freezeUnliftedArray
  , thawUnliftedArray
  , unsafeThawUnliftedArray
  , setUnliftedArray
  , copyUnliftedArray
  , copyMutableUnliftedArray
  , cloneUnliftedArray
  , cloneMutableUnliftedArray
  , emptyUnliftedArray
  , singletonUnliftedArray
  , runUnliftedArray
  , dupableRunUnliftedArray
    -- * List Conversion
  , unliftedArrayToList
  , unliftedArrayFromList
  , unliftedArrayFromListN
    -- * Folding
  , foldrUnliftedArray
  , foldrUnliftedArray'
  , foldlUnliftedArray
  , foldlUnliftedArray'
  , foldlUnliftedArrayM'
    -- * Traversals
  , traverseUnliftedArray_
  , itraverseUnliftedArray_
    -- * Mapping
  , mapUnliftedArray
  ) where

import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.Array.Primops
import GHC.Exts (Int(I#),State#)
import GHC.ST (ST (..))

import qualified Data.List as L
import qualified GHC.Exts as Exts

-- | Using a specialized copy of primitive_ here makes the Core a little
-- easier to read by eliminating unnecessary PrimState coercions.
primitive_ :: (State# s -> State# s) -> ST s ()
{-# INLINE primitive_ #-}
primitive_ m = ST (\s -> (# m s, () #))

-- | An @UnliftedArray_ a unlifted_a@ represents an array of values of a
-- lifted type @a@ that wrap values of an unlifted type @unlifted_a@.
-- It is expected that @unlifted_a ~ Unlifted a@, but imposing that constraint
-- here would force the type roles to @nominal@, which is often undesirable
-- when arrays are used as components of larger datatypes.
data UnliftedArray_ unlifted_a a
  = UnliftedArray (UnliftedArray# unlifted_a)
type role UnliftedArray_ representational phantom 

-- | A type synonym for an 'UnliftedArray_' containing lifted values of
-- a particular type. As a general rule, this type synonym should not be used in
-- class instances—use 'UnliftedArray_' with an equality constraint instead.
-- It also should not be used when defining newtypes or datatypes, unless those
-- will have restrictive type roles regardless—use 'UnliftedArray_' instead.
type UnliftedArray a = UnliftedArray_ (Unlifted a) a

-- | A mutable version of 'UnliftedArray_'.
data MutableUnliftedArray_ unlifted_a s a
  = MutableUnliftedArray (MutableUnliftedArray# s unlifted_a)
type role MutableUnliftedArray_ representational nominal phantom 

-- | A mutable version of 'MutableUnliftedArray'.
type MutableUnliftedArray s a = MutableUnliftedArray_ (Unlifted a) s a

instance unlifted_a ~ Unlifted a => PrimUnlifted (UnliftedArray_ unlifted_a a) where
  type Unlifted (UnliftedArray_ unlifted_a _) = UnliftedArray# unlifted_a
  toUnlifted# (UnliftedArray a) = a
  fromUnlifted# x = UnliftedArray x

instance unlifted_a ~ Unlifted a => PrimUnlifted (MutableUnliftedArray_ unlifted_a s a) where
  type Unlifted (MutableUnliftedArray_ unlifted_a s _) = MutableUnliftedArray# s unlifted_a
  toUnlifted# (MutableUnliftedArray a) = a
  fromUnlifted# x = MutableUnliftedArray x

-- | Creates a new 'MutableUnliftedArray' with the specified value as initial
-- contents.
newUnliftedArray
  :: PrimUnlifted a
  => Int -- ^ size
  -> a -- ^ initial value
  -> ST s (MutableUnliftedArray s a)
newUnliftedArray (I# len) v = ST $ \s -> case newUnliftedArray# len (toUnlifted# v) s of
  (# s', ma #) -> (# s', MutableUnliftedArray ma #)
{-# inline newUnliftedArray #-}

setUnliftedArray
  :: PrimUnlifted a
  => MutableUnliftedArray s a -- ^ destination
  -> a -- ^ value to fill with
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s ()
{-# inline setUnliftedArray #-}
setUnliftedArray mua v off len = loop (len + off - 1)
 where
 loop i
   | i < off = pure ()
   | otherwise = writeUnliftedArray mua i v *> loop (i-1)

-- | Yields the length of an 'UnliftedArray'.
sizeofUnliftedArray :: UnliftedArray e -> Int
{-# inline sizeofUnliftedArray #-}
sizeofUnliftedArray (UnliftedArray ar) = I# (sizeofUnliftedArray# ar)

-- | Yields the length of a 'MutableUnliftedArray'.
sizeofMutableUnliftedArray :: MutableUnliftedArray s e -> Int
{-# inline sizeofMutableUnliftedArray #-}
sizeofMutableUnliftedArray (MutableUnliftedArray maa#)
  = I# (sizeofMutableUnliftedArray# maa#)

writeUnliftedArray :: PrimUnlifted a
  => MutableUnliftedArray s a
  -> Int
  -> a
  -> ST s ()
{-# inline writeUnliftedArray #-}
writeUnliftedArray (MutableUnliftedArray arr) (I# ix) a =
  primitive_ (writeUnliftedArray# arr ix (toUnlifted# a))

readUnliftedArray :: PrimUnlifted a
  => MutableUnliftedArray s a
  -> Int
  -> ST s a
{-# inline readUnliftedArray #-}
readUnliftedArray (MutableUnliftedArray arr) (I# ix) =
  ST $ \s -> case readUnliftedArray# arr ix s of
    (# s', a #) -> (# s', fromUnlifted# a #)

indexUnliftedArray :: PrimUnlifted a
  => UnliftedArray a
  -> Int
  -> a
{-# inline indexUnliftedArray #-}
indexUnliftedArray (UnliftedArray arr) (I# ix) =
  fromUnlifted# (indexUnliftedArray# arr ix)

-- | Freezes a 'MutableUnliftedArray', yielding an 'UnliftedArray'. This simply
-- marks the array as frozen in place, so it should only be used when no further
-- modifications to the mutable array will be performed.
unsafeFreezeUnliftedArray
  :: MutableUnliftedArray s a
  -> ST s (UnliftedArray a)
unsafeFreezeUnliftedArray (MutableUnliftedArray maa#)
  = ST $ \s -> case unsafeFreezeUnliftedArray# maa# s of
      (# s', aa# #) -> (# s', UnliftedArray aa# #)
{-# inline unsafeFreezeUnliftedArray #-}

-- | Determines whether two 'MutableUnliftedArray' values are the same. This is
-- object/pointer identity, not based on the contents.
sameMutableUnliftedArray
  :: MutableUnliftedArray_ unlifted_a s a
  -> MutableUnliftedArray_ unlifted_a s a
  -> Bool
sameMutableUnliftedArray (MutableUnliftedArray maa1#) (MutableUnliftedArray maa2#)
  = Exts.isTrue# (sameMutableUnliftedArray# maa1# maa2#)
{-# inline sameMutableUnliftedArray #-}

-- | Copies the contents of an immutable array into a mutable array.
copyUnliftedArray
  :: MutableUnliftedArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> UnliftedArray a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copyUnliftedArray #-}
copyUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (UnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyUnliftedArray# src soff dst doff ln

-- | Copies the contents of one mutable array into another.
copyMutableUnliftedArray
  :: MutableUnliftedArray s a -- ^ destination
  -> Int -- ^ offset into destination
  -> MutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset into source
  -> Int -- ^ number of elements to copy
  -> ST s ()
{-# inline copyMutableUnliftedArray #-}
copyMutableUnliftedArray
  (MutableUnliftedArray dst) (I# doff)
  (MutableUnliftedArray src) (I# soff) (I# ln) =
    primitive_ $ copyMutableUnliftedArray# src soff dst doff ln

-- | Freezes a portion of a 'MutableUnliftedArray', yielding an 'UnliftedArray'.
-- This operation is safe, in that it copies the frozen portion, and the
-- existing mutable array may still be used afterward.
freezeUnliftedArray
  :: MutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (UnliftedArray a)
freezeUnliftedArray (MutableUnliftedArray mary) (I# off) (I# len) =
    ST $ \s -> case freezeUnliftedArray# mary off len s of
      (# s', ary #) -> (# s', UnliftedArray ary #)
{-# inline freezeUnliftedArray #-}

-- | Thaws a portion of an 'UnliftedArray', yielding a 'MutableUnliftedArray'.
-- This copies the thawed portion, so mutations will not affect the original
-- array.
thawUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (MutableUnliftedArray s a)
{-# inline thawUnliftedArray #-}
thawUnliftedArray (UnliftedArray ary) (I# off) (I# len) =
    ST $ \s -> case thawUnliftedArray# ary off len s of
      (# s', mary #) -> (# s', MutableUnliftedArray mary #)

-- | Thaws an 'UnliftedArray', yielding a 'MutableUnliftedArray'. This
-- does not make a copy.
unsafeThawUnliftedArray
  :: UnliftedArray a -- ^ source
  -> ST s (MutableUnliftedArray s a)
{-# inline unsafeThawUnliftedArray #-}
unsafeThawUnliftedArray (UnliftedArray ary) =
    ST $ \s -> case unsafeThawUnliftedArray# ary s of
      (# s', mary #) -> (# s', MutableUnliftedArray mary #)

-- | Execute a stateful computation and freeze the resulting array.
runUnliftedArray
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray a
{-# INLINE runUnliftedArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- runUnliftedArray m = runST $ noDuplicate >> m >>= unsafeFreezeUnliftedArray
runUnliftedArray m = UnliftedArray (runUnliftedArray# m)

runUnliftedArray#
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray# (Unlifted a)
runUnliftedArray# m = case Exts.runRW# $ \s0 ->
  case Exts.noDuplicate# s0 of { s ->
  case unST m s of { (# s', MutableUnliftedArray mary# #) ->
  unsafeFreezeUnliftedArray# mary# s'}} of (# _, ary# #) -> ary#
{-# INLINE runUnliftedArray# #-}

-- | Execute a stateful computation and freeze the resulting array.
-- It is possible, but unlikely, that the computation will be run
-- multiple times in multiple threads.
dupableRunUnliftedArray
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray a
{-# INLINE dupableRunUnliftedArray #-}
-- This is what we'd like to write, but GHC does not yet
-- produce properly unboxed code when we do
-- runUnliftedArray m = runST $ m >>= unsafeFreezeUnliftedArray
dupableRunUnliftedArray m = UnliftedArray (dupableRunUnliftedArray# m)

dupableRunUnliftedArray#
  :: (forall s. ST s (MutableUnliftedArray s a))
  -> UnliftedArray# (Unlifted a)
dupableRunUnliftedArray# m = case Exts.runRW# $ \s ->
  case unST m s of { (# s', MutableUnliftedArray mary# #) ->
  unsafeFreezeUnliftedArray# mary# s'} of (# _, ary# #) -> ary#
{-# INLINE dupableRunUnliftedArray# #-}

unST :: ST s a -> State# s -> (# State# s, a #)
unST (ST f) = f

unsafeCreateUnliftedArray
  :: Int
  -> (forall s. MutableUnliftedArray s a -> ST s ())
  -> UnliftedArray a
unsafeCreateUnliftedArray !n f = runUnliftedArray $ do
  mary <- unsafeNewUnliftedArray n
  f mary
  pure mary

-- | Creates a new 'MutableUnliftedArray'. This function is unsafe because it
-- initializes all elements of the array as pointers to the empty array. Attempting
-- to read one of these elements before writing to it is in effect an unsafe
-- coercion from @'UnliftedArray' a@ to the element type.
unsafeNewUnliftedArray
  :: Int -- ^ size
  -> ST s (MutableUnliftedArray s a)
{-# inline unsafeNewUnliftedArray #-}
unsafeNewUnliftedArray (I# i) = ST $ \s -> case unsafeNewUnliftedArray# i s of
  (# s', ma #) -> (# s', MutableUnliftedArray ma #)


-- | Creates a copy of a portion of an 'UnliftedArray'
cloneUnliftedArray
  :: UnliftedArray a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> UnliftedArray a
{-# inline cloneUnliftedArray #-}
cloneUnliftedArray (UnliftedArray ary) (I# off) (I# len)
  = UnliftedArray (cloneUnliftedArray# ary off len)

-- | Creates a new 'MutableUnliftedArray' containing a copy of a portion of
-- another mutable array.
cloneMutableUnliftedArray
  :: MutableUnliftedArray s a -- ^ source
  -> Int -- ^ offset
  -> Int -- ^ length
  -> ST s (MutableUnliftedArray s a)
{-# inline cloneMutableUnliftedArray #-}
cloneMutableUnliftedArray (MutableUnliftedArray mary) (I# off) (I# len)
  = ST $ \s -> case cloneMutableUnliftedArray# mary off len s of
      (# s', mary' #) -> (# s', MutableUnliftedArray mary' #)

emptyUnliftedArray :: UnliftedArray_ unlifted_a a
emptyUnliftedArray = UnliftedArray (emptyUnliftedArray# (##))

singletonUnliftedArray :: PrimUnlifted a => a -> UnliftedArray a
{-# INLINE singletonUnliftedArray #-}
singletonUnliftedArray x = dupableRunUnliftedArray $ newUnliftedArray 1 x

concatUnliftedArray :: UnliftedArray a -> UnliftedArray a -> UnliftedArray a
{-# INLINE concatUnliftedArray #-}
concatUnliftedArray (UnliftedArray a1) (UnliftedArray a2)
  = UnliftedArray (concatUnliftedArray# a1 a2)

-- This junk is to make sure we unbox properly. Inlining this doesn't seem
-- likely to be much of a win ever, and could potentially lead to reboxing,
-- so we NOINLINE. It would be nice to find a prettier way to do this.
concatUnliftedArray# :: UnliftedArray# a -> UnliftedArray# a -> UnliftedArray# a
{-# NOINLINE concatUnliftedArray# #-}
concatUnliftedArray# a1 a2 =
  let !sza1 = sizeofUnliftedArray# a1
  in
    if Exts.isTrue# (sza1 Exts.==# 0#)
    then a2
    else
      let !sza2 = sizeofUnliftedArray# a2
      in
        if Exts.isTrue# (sza2 Exts.==# 0#)
        then a1
        else Exts.runRW# $ \s0 ->
          let
            finish s =
              case unsafeNewUnliftedArray# (sza1 Exts.+# sza2) s of { (# s', ma #) ->
              case copyUnliftedArray# a1 0# ma 0# sza1 s' of { s'' ->
              case copyUnliftedArray# a2 0# ma sza1 sza2 s'' of { s''' ->
              case unsafeFreezeUnliftedArray# ma s''' of
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

foldrUnliftedArray :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
{-# INLINE foldrUnliftedArray #-}
foldrUnliftedArray f z arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | sz > i = f (indexUnliftedArray arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of an 'UnliftedArray.
{-# INLINE foldrUnliftedArray' #-}
foldrUnliftedArray' :: forall a b. PrimUnlifted a => (a -> b -> b) -> b -> UnliftedArray a -> b
foldrUnliftedArray' f z0 arr = go (sizeofUnliftedArray arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (indexUnliftedArray arr i) acc)

-- | Lazy left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray #-}
foldlUnliftedArray :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray f z arr = go (sizeofUnliftedArray arr - 1)
  where
    go !i
      | i < 0 = z
      | otherwise = f (go (i - 1)) (indexUnliftedArray arr i)

-- | Strict left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArray' #-}
foldlUnliftedArray' :: forall a b. PrimUnlifted a => (b -> a -> b) -> b -> UnliftedArray a -> b
foldlUnliftedArray' f z0 arr = go 0 z0
  where
    !sz = sizeofUnliftedArray arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (indexUnliftedArray arr i))
      | otherwise = acc

-- | Strict effectful left-associated fold over the elements of an 'UnliftedArray'.
{-# INLINE foldlUnliftedArrayM' #-}
foldlUnliftedArrayM' :: (PrimUnlifted a, Monad m)
  => (b -> a -> m b) -> b -> UnliftedArray a -> m b
foldlUnliftedArrayM' f z0 arr = go 0 z0
  where
    !sz = sizeofUnliftedArray arr
    go !i !acc
      | i < sz = f acc (indexUnliftedArray arr i) >>= go (i + 1) 
      | otherwise = pure acc

-- | Effectfully traverse the elements of an 'UnliftedArray', discarding
-- the resulting values.
{-# INLINE traverseUnliftedArray_ #-}
traverseUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (a -> m b) -> UnliftedArray a -> m ()
traverseUnliftedArray_ f arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | i < sz = f (indexUnliftedArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Effectful indexed traversal of the elements of an 'UnliftedArray',
-- discarding the resulting values.
{-# INLINE itraverseUnliftedArray_ #-}
itraverseUnliftedArray_ :: (PrimUnlifted a, Applicative m)
  => (Int -> a -> m b) -> UnliftedArray a -> m ()
itraverseUnliftedArray_ f arr = go 0
  where
    !sz = sizeofUnliftedArray arr
    go !i
      | i < sz = f i (indexUnliftedArray arr i) *> go (i + 1) 
      | otherwise = pure ()

-- | Map over the elements of an 'UnliftedArray'.
{-# INLINE mapUnliftedArray #-}
mapUnliftedArray :: (PrimUnlifted a, PrimUnlifted b)
  => (a -> b)
  -> UnliftedArray a
  -> UnliftedArray b
-- TODO: Do we need unsafeCreateUnliftedArray here, or would it be better
-- to use a hypothetical unsafeDupableCreateUnliftedArray? I don't have
-- much intuition for this. On one hand, if the operation creates a
-- bunch of expensive objects to stick in the array, then we really don't want
-- to duplicate that work. On the other hand, it's likely that creating
-- a bunch of expensive objects will also allocate a bunch of memory, which
-- will likely trigger garbage collection that (as I understand it) will
-- notice that one thunk is being evaluated twice and deduplicate. On the
-- other other hand, I don't think there's any guarantee that the thread that wins will be
-- the one that's further along, so maybe the noDuplicate is for the best.
mapUnliftedArray f arr = unsafeCreateUnliftedArray sz $ \marr -> do
  let go !ix = if ix < sz
        then do
          let b = f (indexUnliftedArray arr ix)
          writeUnliftedArray marr ix b
          go (ix + 1)
        else return ()
  go 0
  where
  !sz = sizeofUnliftedArray arr

-- | Convert the unlifted array to a list.
{-# INLINE unliftedArrayToList #-}
unliftedArrayToList :: PrimUnlifted a => UnliftedArray a -> [a]
unliftedArrayToList xs = Exts.build (\c n -> foldrUnliftedArray c n xs)

unliftedArrayFromList :: PrimUnlifted a => [a] -> UnliftedArray a
unliftedArrayFromList xs = unliftedArrayFromListN (L.length xs) xs

unliftedArrayFromListN :: forall a. PrimUnlifted a => Int -> [a] -> UnliftedArray a
unliftedArrayFromListN len vs = unsafeCreateUnliftedArray len run where
  run :: forall s. MutableUnliftedArray s a -> ST s ()
  run arr = do
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          -- The size check is mandatory since failure to initialize all elements
          -- introduces the possibility of a segfault happening when someone attempts
          -- to read the unitialized element. See the docs for unsafeNewUnliftedArray.
          then return ()
          else die "unliftedArrayFromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            writeUnliftedArray arr ix a
            go as (ix + 1)
          else die "unliftedArrayFromListN" "list length greater than specified size"
    go vs 0

instance (PrimUnlifted a, unlifted_a ~ Unlifted a)
  => Exts.IsList (UnliftedArray_ unlifted_a a) where
  type Item (UnliftedArray_ _ a) = a
  fromList = unliftedArrayFromList
  fromListN = unliftedArrayFromListN
  toList = unliftedArrayToList

instance (PrimUnlifted a, unlifted_a ~ Unlifted a)
  => Semigroup (UnliftedArray_ unlifted_a a) where
  (<>) = concatUnliftedArray

instance (PrimUnlifted a, unlifted_a ~ Unlifted a) => Monoid (UnliftedArray_ unlifted_a a) where
  mempty = emptyUnliftedArray

instance (Show a, PrimUnlifted a, unlifted_a ~ Unlifted a) => Show (UnliftedArray_ unlifted_a a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (sizeofUnliftedArray a) . showString " "
      . shows (unliftedArrayToList a)

instance unlifted_a ~ Unlifted a => Eq (MutableUnliftedArray_ unlifted_a s a) where
  (==) = sameMutableUnliftedArray

instance (Eq a, PrimUnlifted a, unlifted_a ~ Unlifted a) => Eq (UnliftedArray_ unlifted_a a) where
  aa1 == aa2 = sizeofUnliftedArray aa1 == sizeofUnliftedArray aa2
            && loop (sizeofUnliftedArray aa1 - 1)
   where
   loop i
     | i < 0 = True
     | otherwise = indexUnliftedArray aa1 i == indexUnliftedArray aa2 i && loop (i-1)

die :: String -> String -> a
die fun problem = error $ "Data.Primitive.UnliftedArray.ST." ++ fun ++ ": " ++ problem
