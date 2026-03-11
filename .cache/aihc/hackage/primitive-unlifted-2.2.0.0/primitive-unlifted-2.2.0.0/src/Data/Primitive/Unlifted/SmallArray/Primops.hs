{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language RoleAnnotations #-}
{-# language UnliftedNewtypes #-}
{-# language KindSignatures #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneKindSignatures #-}
{-# language DataKinds #-}
{-# language UnliftedDatatypes #-}

-- |
-- Primitive types representing unlifted arrays and the
-- primops for manipulating them.
module Data.Primitive.Unlifted.SmallArray.Primops
  ( -- * Types
    SmallUnliftedArray#(..)
  , SmallMutableUnliftedArray#(..)
    -- We don't export the newtype constructors because they're bogus and
    -- because there's basically no reason they'd ever be used. This module
    -- contains a wrapped version of every Array# primop.  Eventually, all this
    -- stuff will be in GHC.Prim, possibly with other names.

    -- * Operations
  , newSmallUnliftedArray#
  , unsafeNewSmallUnliftedArray#
  , emptySmallUnliftedArray#
  , sameSmallMutableUnliftedArray#
  , shrinkSmallMutableUnliftedArray#
  , readSmallUnliftedArray#
  , writeSmallUnliftedArray#
  , sizeofSmallUnliftedArray#
  , getSizeofSmallMutableUnliftedArray#
  , indexSmallUnliftedArray#
  , unsafeFreezeSmallUnliftedArray#
  , unsafeThawSmallUnliftedArray#
  , copySmallUnliftedArray#
  , copySmallMutableUnliftedArray#
  , cloneSmallUnliftedArray#
  , cloneSmallMutableUnliftedArray#
  , freezeSmallUnliftedArray#
  , thawSmallUnliftedArray#
  , casSmallUnliftedArray#
  ) where

import Data.Coerce (coerce)
import GHC.Exts (Int#,State#,SmallArray#,SmallMutableArray#)
import qualified GHC.Exts as Exts

import Data.Primitive.Unlifted.Type
import Unsafe.Coerce (unsafeCoerceUnlifted)

newtype SmallUnliftedArray# (a :: UnliftedType) = SmallUnliftedArray# (SmallArray# a)
type role SmallUnliftedArray# representational

newtype SmallMutableUnliftedArray# s (a :: UnliftedType) = SmallMutableUnliftedArray# (SmallMutableArray# s a)
type role SmallMutableUnliftedArray# nominal representational

newSmallUnliftedArray# :: forall a s. Int# -> a -> State# s -> (# State# s, SmallMutableUnliftedArray# s a #)
newSmallUnliftedArray# sz a s = coerce (Exts.newSmallArray# sz a s)
{-# INLINE newSmallUnliftedArray# #-}

-- | Create a 'SmallMutableUnliftedArray#' whose entries contain some unspecified
-- static value. This may be more convenient than 'newUnliftedArray#' if there
-- is no value on hand with which to initialize the array. Each entry must be
-- initialized before being read and used. This condition is not checked.
unsafeNewSmallUnliftedArray# :: Int# -> State# s -> (# State# s, SmallMutableUnliftedArray# s a #)
-- We fill the array with the Nonsense data constructor. It doesn't much matter
-- *what* we stick in there, as long as it's a pointer the garbage collector
-- can understand and isn't something that might otherwise be released as garbage.
-- There's no point trying to stick an `error` in there, because there's no
-- code anywhere to force the error thunk.
unsafeNewSmallUnliftedArray# sz s = case Exts.newSmallArray# sz (unsafeCoerceUnlifted Nonsense) s of
  (# s', mary #) -> (# s', SmallMutableUnliftedArray# mary #)
{-# INLINE unsafeNewSmallUnliftedArray# #-}

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
empty_small_unlifted_array :: SULA a
empty_small_unlifted_array = SULA
  (Exts.runRW# $ \s ->
    case Exts.noDuplicate# s of { s' ->
    case Exts.newSmallArray# 0# (unsafeCoerceUnlifted Nonsense) s' of { (# s'', mary #) ->
    case Exts.unsafeFreezeSmallArray# mary s'' of { (# _, ary #) ->
      SmallUnliftedArray# ary }}})
{-# NOINLINE empty_small_unlifted_array #-}

data SULA a = SULA (SmallUnliftedArray# a)

-- | Warning: Applying 'unsafeThawUnliftedArray#' to the array produced by
-- this function will make demons come out of your nose.
emptySmallUnliftedArray# :: (##) -> SmallUnliftedArray# a
-- We make this primitive because it's the easiest way to get a
-- *shared* primitive unlifted array.
--
-- Why the stern warning above? GHC does not currently support resizing 'Array#',
-- and does not really meaningfully support *growing* arrays of any type. If,
-- however, that ever changes, growing the globally shared empty array would be
-- pretty disastrous.
emptySmallUnliftedArray# (##) = case empty_small_unlifted_array of
  SULA ary -> ary
{-# INLINE emptySmallUnliftedArray# #-}

sameSmallMutableUnliftedArray# :: SmallMutableUnliftedArray# s a -> SmallMutableUnliftedArray# s a -> Int#
sameSmallMutableUnliftedArray# (SmallMutableUnliftedArray# ar1) (SmallMutableUnliftedArray# ar2)
  = Exts.reallyUnsafePtrEquality# ar1 ar2
{-# INLINE sameSmallMutableUnliftedArray# #-}

shrinkSmallMutableUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> State# s -> State# s
shrinkSmallMutableUnliftedArray# (SmallMutableUnliftedArray# ar) sz s
  = Exts.shrinkSmallMutableArray# ar sz s
{-# INLINE shrinkSmallMutableUnliftedArray# #-}

readSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> State# s -> (# State# s, a #)
readSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i s
  = Exts.readSmallArray# mary i s
{-# INLINE readSmallUnliftedArray# #-}

writeSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> a -> State# s -> State# s
writeSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i a s
  = Exts.writeSmallArray# mary i a s
{-# INLINE writeSmallUnliftedArray# #-}

sizeofSmallUnliftedArray# :: SmallUnliftedArray# a -> Int#
sizeofSmallUnliftedArray# (SmallUnliftedArray# ary) = Exts.sizeofSmallArray# ary
{-# INLINE sizeofSmallUnliftedArray# #-}

getSizeofSmallMutableUnliftedArray# :: SmallMutableUnliftedArray# s a -> State# s -> (# State# s, Int# #)
getSizeofSmallMutableUnliftedArray# (SmallMutableUnliftedArray# mary) s
  = Exts.getSizeofSmallMutableArray# mary s
{-# INLINE getSizeofSmallMutableUnliftedArray# #-}

{-
--The underlying primop is deprecated in GHC.Prim, so let's not do this.
sizeofSmallMutableUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int#
sizeofSmallMutableUnliftedArray# (SmallMutableUnliftedArray# mary)
  = Exts.sizeofSmallMutableArray# mary
{-# INLINE sizeofSmallMutableUnliftedArray# #-}
-}

indexSmallUnliftedArray# :: SmallUnliftedArray# a -> Int# -> a
indexSmallUnliftedArray# (SmallUnliftedArray# ary) i
  | (# a #) <- Exts.indexSmallArray# ary i
  = a
{-# INLINE indexSmallUnliftedArray# #-}

unsafeFreezeSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> State# s -> (# State# s, SmallUnliftedArray# a #)
unsafeFreezeSmallUnliftedArray# (SmallMutableUnliftedArray# mary) s
  = coerce (Exts.unsafeFreezeSmallArray# mary s)
{-# INLINE unsafeFreezeSmallUnliftedArray# #-}

unsafeThawSmallUnliftedArray# :: SmallUnliftedArray# a -> State# s -> (# State# s, SmallMutableUnliftedArray# s a #)
unsafeThawSmallUnliftedArray# (SmallUnliftedArray# ary) s
  = coerce (Exts.unsafeThawSmallArray# ary s)
{-# INLINE unsafeThawSmallUnliftedArray# #-}

copySmallUnliftedArray# :: SmallUnliftedArray# a -> Int# -> SmallMutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copySmallUnliftedArray# (SmallUnliftedArray# ary) i1 (SmallMutableUnliftedArray# mary) i2 n s
  = Exts.copySmallArray# ary i1 mary i2 n s
{-# INLINE copySmallUnliftedArray# #-}

copySmallMutableUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> SmallMutableUnliftedArray# s a -> Int# -> Int# -> State# s -> State# s
copySmallMutableUnliftedArray# (SmallMutableUnliftedArray# mary1) i1 (SmallMutableUnliftedArray# mary2) i2 n s
  = Exts.copySmallMutableArray# mary1 i1 mary2 i2 n s
{-# INLINE copySmallMutableUnliftedArray# #-}

cloneSmallUnliftedArray# :: SmallUnliftedArray# a -> Int# -> Int# -> SmallUnliftedArray# a
cloneSmallUnliftedArray# (SmallUnliftedArray# ary) i n
  = SmallUnliftedArray# (Exts.cloneSmallArray# ary i n)
{-# INLINE cloneSmallUnliftedArray# #-}

cloneSmallMutableUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> Int# -> State# s
  -> (# State# s, SmallMutableUnliftedArray# s a #)
cloneSmallMutableUnliftedArray# (SmallMutableUnliftedArray# mary) i n s
  = coerce (Exts.cloneSmallMutableArray# mary i n s)
{-# INLINE cloneSmallMutableUnliftedArray# #-}

freezeSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallUnliftedArray# a #)
freezeSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i n s
  = coerce (Exts.freezeSmallArray# mary i n s)
{-# INLINE freezeSmallUnliftedArray# #-}

thawSmallUnliftedArray# :: SmallUnliftedArray# a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableUnliftedArray# s a #)
thawSmallUnliftedArray# (SmallUnliftedArray# ary) i n s
  = coerce (Exts.thawSmallArray# ary i n s)
{-# INLINE thawSmallUnliftedArray# #-}

casSmallUnliftedArray# :: SmallMutableUnliftedArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
casSmallUnliftedArray# (SmallMutableUnliftedArray# mary) i x y s
  = Exts.casSmallArray# mary i x y s
{-# INLINE casSmallUnliftedArray# #-}
