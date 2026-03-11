{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
{-# language StandaloneKindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}
{-# language UnliftedDatatypes #-}

-- Oh what a mess this is! See UnsafeCoercions.md for an explanation
-- of the hodgepodge in this module.

-- |
-- Primitive types representing unlifted arrays and the
-- primops for manipulating them.
module Data.Primitive.Unlifted.Array.Primops
  ( -- * Types
    UnliftedArray#(..)
  , MutableUnliftedArray#(..)

    -- * Operations
  , newUnliftedArray#
  , unsafeNewUnliftedArray#
  , emptyUnliftedArray#
  , sameMutableUnliftedArray#
  , readUnliftedArray#
  , writeUnliftedArray#
  , sizeofUnliftedArray#
  , sizeofMutableUnliftedArray#
  , indexUnliftedArray#
  , unsafeFreezeUnliftedArray#
  , unsafeThawUnliftedArray#
  , copyUnliftedArray#
  , copyMutableUnliftedArray#
  , cloneUnliftedArray#
  , cloneMutableUnliftedArray#
  , freezeUnliftedArray#
  , thawUnliftedArray#
  , casUnliftedArray#
  ) where

import Data.Coerce (coerce)
import GHC.Exts ( Int#, State#, Array#, MutableArray# )
import qualified GHC.Exts as Exts

import Data.Primitive.Unlifted.Type
import Unsafe.Coerce (unsafeCoerceUnlifted)

newtype UnliftedArray# (a :: UnliftedType) = UnliftedArray# (Array# a)
type role UnliftedArray# representational

newtype MutableUnliftedArray# s (a :: UnliftedType) = MutableUnliftedArray# (MutableArray# s a)
type role MutableUnliftedArray# nominal representational

newUnliftedArray# :: Int# -> a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
newUnliftedArray# sz a s = coerce (Exts.newArray# sz a s)
{-# INLINE newUnliftedArray# #-}

-- | Create a 'MutableUnliftedArray#' whose entries contain some unspecified
-- static value. This may be more convenient than 'newUnliftedArray#' if there
-- is no value on hand with which to initialize the array. Each entry must be
-- initialized before being read and used. This condition is not checked.
unsafeNewUnliftedArray# :: Int# -> State# s -> (# State# s, MutableUnliftedArray# s a #)
unsafeNewUnliftedArray# sz s
  | (# s', mary #) <- Exts.newArray# sz (unsafeCoerceUnlifted Nonsense) s
  = (# s', MutableUnliftedArray# mary #)
{-# INLINE unsafeNewUnliftedArray# #-}

type Nonsense :: UnliftedType
data Nonsense = Nonsense

-- This represents a *statically allocated* value, preferably in a *read-only*
-- segment of memory.
--
-- Why do we bother to noDuplicate#? It generally doesn't much *matter* if
-- different threads have different global empty arrays. However, for
-- performance testing purposes, a user may well want to check whether the
-- empty arrays they expect to be the global ones really are. Such a test
-- is only possible if there's just *one* array to test against. The overhead
-- of the once-ever noDuplicate# call is sure to be trivial anyway.
empty_unlifted_array :: ULA a
empty_unlifted_array = ULA
  (Exts.runRW# $ \s ->
    case Exts.noDuplicate# s of { s' ->
    case unsafeNewUnliftedArray# 0# s' of { (# s'', mary #) ->
    case unsafeFreezeUnliftedArray# mary s'' of { (# _, ary #) ->
      ary }}})
{-# NOINLINE empty_unlifted_array #-}

data ULA a = ULA (UnliftedArray# a)

-- | Warning: Applying 'unsafeThawUnliftedArray#' to the array produced by
-- this function will make demons come out of your nose.
emptyUnliftedArray# :: (##) -> UnliftedArray# a
-- We make this primitive because it's the easiest way to get a
-- *shared* primitive unlifted array.
--
-- Why the stern warning above? GHC does not currently support resizing 'Array#',
-- and does not really meaningfully support *growing* arrays of any type. If,
-- however, that ever changes, growing the globally shared empty array would be
-- pretty disastrous.
emptyUnliftedArray# (##) = case empty_unlifted_array of
  ULA ary -> ary
{-# INLINE emptyUnliftedArray# #-}

sameMutableUnliftedArray# :: MutableUnliftedArray# s a -> MutableUnliftedArray# s a -> Int#
sameMutableUnliftedArray# (MutableUnliftedArray# ar1) (MutableUnliftedArray# ar2)
  = Exts.reallyUnsafePtrEquality# ar1 ar2
{-# INLINE sameMutableUnliftedArray# #-}

readUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readUnliftedArray# (MutableUnliftedArray# mary) i s
  = coerce (Exts.readArray# mary i s)
{-# INLINE readUnliftedArray# #-}

writeUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
writeUnliftedArray# (MutableUnliftedArray# mary) i a s
  = Exts.writeArray# mary i a s
{-# INLINE writeUnliftedArray# #-}

sizeofUnliftedArray# :: UnliftedArray# a -> Int#
sizeofUnliftedArray# (UnliftedArray# ary) = Exts.sizeofArray# ary
{-# INLINE sizeofUnliftedArray# #-}

sizeofMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int#
sizeofMutableUnliftedArray# (MutableUnliftedArray# mary)
  = Exts.sizeofMutableArray# mary
{-# INLINE sizeofMutableUnliftedArray# #-}

indexUnliftedArray# :: UnliftedArray# a -> Int# -> a
indexUnliftedArray# (UnliftedArray# ary) i
  = case Exts.indexArray# ary i of (# a #) -> a
{-# INLINE indexUnliftedArray# #-}

unsafeFreezeUnliftedArray# :: MutableUnliftedArray# s a -> State# s -> (# State# s, UnliftedArray# a #)
unsafeFreezeUnliftedArray# (MutableUnliftedArray# mary) s
  = case Exts.unsafeFreezeArray# mary s of
      (# s', ary #) -> (# s', UnliftedArray# ary #)
{-# INLINE unsafeFreezeUnliftedArray# #-}

unsafeThawUnliftedArray# :: UnliftedArray# a -> State# s -> (# State# s, MutableUnliftedArray# s a #)
unsafeThawUnliftedArray# (UnliftedArray# ary) s
  = case Exts.unsafeThawArray# ary s of
     (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE unsafeThawUnliftedArray# #-}

copyUnliftedArray# :: UnliftedArray# a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copyUnliftedArray# (UnliftedArray# ary) i1 (MutableUnliftedArray# mary) i2 n s
  = Exts.copyArray# ary i1 mary i2 n s
{-# INLINE copyUnliftedArray# #-}

copyMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copyMutableUnliftedArray# (MutableUnliftedArray# mary1) i1 (MutableUnliftedArray# mary2) i2 n s
  = Exts.copyMutableArray# mary1 i1 mary2 i2 n s
{-# INLINE copyMutableUnliftedArray# #-}

cloneUnliftedArray# :: UnliftedArray# a -> Int# -> Int# -> UnliftedArray# a
cloneUnliftedArray# (UnliftedArray# ary) i n
  = UnliftedArray# (Exts.cloneArray# ary i n)
{-# INLINE cloneUnliftedArray# #-}

cloneMutableUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> Int# -> State# s
  -> (# State# s, MutableUnliftedArray# s a #)
cloneMutableUnliftedArray# (MutableUnliftedArray# mary) i n s
  = case Exts.cloneMutableArray# mary i n s of
      (# s', mary' #) -> (# s', MutableUnliftedArray# mary' #)
{-# INLINE cloneMutableUnliftedArray# #-}

freezeUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> Int# -> State# s -> (# State# s, UnliftedArray# a #)
freezeUnliftedArray# (MutableUnliftedArray# mary) i n s
  = case Exts.freezeArray# mary i n s of
      (# s', ary #) -> (# s', UnliftedArray# ary #)
{-# INLINE freezeUnliftedArray# #-}

thawUnliftedArray# :: UnliftedArray# a -> Int# -> Int# -> State# s -> (# State# s, MutableUnliftedArray# s a #)
thawUnliftedArray# (UnliftedArray# ary) i n s
  = case Exts.thawArray# ary i n s of
      (# s', mary #) -> (# s', MutableUnliftedArray# mary #)
{-# INLINE thawUnliftedArray# #-}

casUnliftedArray# :: MutableUnliftedArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
casUnliftedArray# (MutableUnliftedArray# mary) i x y s
  = coerce (Exts.casArray# mary i x y s)
{-# INLINE casUnliftedArray# #-}
