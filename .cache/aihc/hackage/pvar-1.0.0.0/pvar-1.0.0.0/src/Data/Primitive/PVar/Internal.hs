{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
-- |
-- Module      : Data.Primitive.PVar.Internal
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar.Internal
  ( PVar(..)
  , newPVar
  , newPinnedPVar
  , newAlignedPinnedPVar
  , rawPVar
  , rawPinnedPVar
  , rawAlignedPinnedPVar
  , rawStorablePVar
  , rawAlignedStorablePVar
  , unsafeToPtrPVar
  , runWithPokedPtr
  , peekPrim
  , pokePrim
  , readPVar
  , writePVar
  , isPinnedPVar
  , sizeOfPVar
  , sizeOfPVar#
  , alignmentPVar
  , alignmentPVar#
  , unI#
  -- * Atomic operations
  , atomicModifyIntArray#
  , atomicModifyIntPVar
  , atomicModifyIntArray_#
  , atomicModifyIntPVar_
  -- * Re-exports
  , isByteArrayPinned#
  , isMutableByteArrayPinned#
  , sizeOf
  , alignment
  )
  where

import Control.DeepSeq
import Control.Monad.Primitive (MonadPrim, primitive, primitive_,
                                touch, unsafePrimToPrim)
import Data.Primitive.Types
import qualified Foreign.Storable as S
import GHC.Exts

#if !MIN_VERSION_primitive(0,6,3)
import Data.Primitive (sizeOf, alignment)
#endif

-- | Mutable variable with primitive value.
--
-- @since 0.1.0
data PVar a s = PVar (MutableByteArray# s)

-- | @`S.poke`+`S.peek`@ will result in a new copy of a `PVar`
instance Prim a => S.Storable (PVar a RealWorld) where
  sizeOf = sizeOfPVar
  {-# INLINE sizeOf #-}
  alignment = alignmentPVar
  {-# INLINE alignment #-}
  peekElemOff (Ptr addr#) (I# i#) = do
    a <- primitive (readOffAddr# addr# i#)
    newAlignedPinnedPVar a
  {-# INLINE peekElemOff #-}
  pokeElemOff (Ptr addr#) (I# i#) pvar = do
    a <- readPVar pvar
    primitive_ (writeOffAddr# addr# i# a)
  {-# INLINE pokeElemOff #-}

-- | Values are already written into `PVar` in NF, this instance is trivial.
instance NFData (PVar a s) where
  rnf (PVar _) = ()

-- | Create a mutable variable in unpinned memory (i.e. GC can move it) with an initial
-- value. This is a prefered way to create a mutable variable, since it will not
-- contribute to memory fragmentation. For pinned memory versions see `newPinnedPVar` and
-- `newAlignedPinnedPVar`
--
-- @since 0.1.0
newPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newPVar v = do
  pvar <- rawPVar
  writePVar pvar v
  return pvar
{-# INLINE newPVar #-}

-- | Create a mutable variable in unpinned and unititialized memory
--
-- @since 0.1.0
rawPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
rawPVar =
  primitive $ \s# ->
    case newByteArray# (sizeOf# (undefined :: a)) s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawPVar #-}


-- | Create a mutable variable in pinned memory with an initial value.
--
-- @since 0.1.0
newPinnedPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newPinnedPVar v = do
  pvar <- rawPinnedPVar
  writePVar pvar v
  return pvar
{-# INLINE newPinnedPVar #-}

-- | Create a mutable variable in pinned memory with uninitialized memory.
--
-- @since 0.1.0
rawPinnedPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
rawPinnedPVar =
  primitive $ \s# ->
    case newPinnedByteArray# (sizeOf# (undefined :: a)) s# of
      (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawPinnedPVar #-}


-- | Create a mutable variable in pinned memory with an initial value and aligned
-- according to its `Data.Primitive.Types.alignment`
--
-- @since 0.1.0
newAlignedPinnedPVar :: (MonadPrim s m, Prim a) => a -> m (PVar a s)
newAlignedPinnedPVar v = do
  pvar <- rawAlignedPinnedPVar
  writePVar pvar v
  return pvar
{-# INLINE newAlignedPinnedPVar #-}


-- | Create a mutable variable in pinned uninitialized memory.
--
-- @since 0.1.0
rawAlignedPinnedPVar ::
     forall a m s. (MonadPrim s m, Prim a)
  => m (PVar a s)
rawAlignedPinnedPVar =
  let dummy = undefined :: a
   in primitive $ \s# ->
        case newAlignedPinnedByteArray# (sizeOf# dummy) (alignment# dummy) s# of
          (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawAlignedPinnedPVar #-}

-- | Create a mutable variable in pinned uninitialized memory using Storable interface for
-- getting the number of bytes for memory allocation.
--
-- @since 0.1.0
rawStorablePVar ::
     forall a m s. (MonadPrim s m, S.Storable a)
  => m (PVar a s)
rawStorablePVar =
  case S.sizeOf (undefined :: a) of
    I# size# ->
      primitive $ \s# ->
        case newPinnedByteArray# size# s# of
          (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawStorablePVar #-}

-- | Create a mutable variable in pinned uninitialized memory using Storable interface for
-- getting the number of bytes for memory allocation and alignement.
--
-- @since 0.1.0
rawAlignedStorablePVar ::
     forall a m s. (MonadPrim s m, S.Storable a)
  => m (PVar a s)
rawAlignedStorablePVar =
  let dummy = undefined :: a
   in case S.sizeOf dummy of
        I# size# ->
          case S.alignment dummy of
            I# align# ->
              primitive $ \s# ->
                case newAlignedPinnedByteArray# size# align# s# of
                  (# s'#, mba# #) -> (# s'#, PVar mba# #)
{-# INLINE rawAlignedStorablePVar #-}


-- | Get the address to the contents. This is highly unsafe, espcially if memory is not pinned
--
-- @since 0.1.0
unsafeToPtrPVar :: PVar a s -> Ptr a
unsafeToPtrPVar (PVar mba#) = Ptr (byteArrayContents# (unsafeCoerce# mba#))
{-# INLINE unsafeToPtrPVar #-}

-- helper that filles the PVar before running the action
runWithPokedPtr ::
     (S.Storable a, MonadPrim s m)
  => PVar a s
  -> a
  -> (PVar a s -> Ptr a -> m b)
  -> m b
runWithPokedPtr pvar a f = do
  let ptr = unsafeToPtrPVar pvar
  pokePrim ptr a
  r <- f pvar ptr
  touch pvar
  return r
{-# INLINE runWithPokedPtr #-}


-- | Use `S.Storable` reading functionality inside the `PrimMonad`.
--
-- @since 0.1.0
peekPrim :: (S.Storable a, MonadPrim s m) => Ptr a -> m a
peekPrim = unsafePrimToPrim . S.peek
{-# INLINE peekPrim #-}

-- | Use `S.Storable` wrting functionality inside the `PrimMonad`.
--
-- @since 0.1.0
pokePrim :: (S.Storable a, MonadPrim s m) => Ptr a -> a -> m ()
pokePrim ptr = unsafePrimToPrim . S.poke ptr
{-# INLINE pokePrim #-}

-- | Read a value from a mutable variable
--
-- @since 0.1.0
readPVar :: (MonadPrim s m, Prim a) => PVar a s -> m a
readPVar (PVar mba#) = primitive (readByteArray# mba# 0#)
{-# INLINE readPVar #-}

-- | Write a value into a mutable variable
--
-- @since 0.1.0
writePVar :: (MonadPrim s m, Prim a) => PVar a s -> a -> m ()
writePVar (PVar mba#) v = primitive_ (writeByteArray# mba# 0# v)
{-# INLINE writePVar #-}

-- | Get the size of the mutable variable in bytes as an unpacked integer
--
-- @since 0.1.0
sizeOfPVar# :: forall a s. Prim a => PVar a s -> Int#
sizeOfPVar# _ = sizeOf# (undefined :: a)
{-# INLINE sizeOfPVar# #-}

-- | Get the alignment of the mutable variable in bytes as an unpacked integer
--
-- @since 0.1.0
alignmentPVar# :: forall a s. Prim a => PVar a s -> Int#
alignmentPVar# _ = alignment# (undefined :: a)
{-# INLINE alignmentPVar# #-}


-- | Size in bytes of a value stored inside the mutable variable. `PVar` itself is neither
-- accessed nor evaluated.
--
-- @since 0.1.0
sizeOfPVar :: Prim a => PVar a s -> Int
sizeOfPVar pvar = I# (sizeOfPVar# pvar)
{-# INLINE sizeOfPVar #-}

-- | Alignment in bytes of the value stored inside of the mutable variable. `PVar` itself is
-- neither accessed nor evaluated.
--
-- @since 0.1.0
alignmentPVar :: Prim a => PVar a s -> Int
alignmentPVar pvar = I# (alignmentPVar# pvar)
{-# INLINE alignmentPVar #-}

-- | Unwrap the primitive `Int`
--
-- @since 0.1.0
unI# :: Int -> Int#
unI# (I# i#) = i#
{-# INLINE unI# #-}



-- | Check if `PVar` is backed by pinned memory or not
--
-- @since 0.1.0
isPinnedPVar :: PVar a s -> Bool
isPinnedPVar (PVar mba#) = isTrue# (isMutableByteArrayPinned# mba#)
{-# INLINE isPinnedPVar #-}


-- | Using `casIntArray#` perform atomic modification of an integer element in a
-- `MutableByteArray#`. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyIntArray# ::
     MutableByteArray# d -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (Int# -> (# Int#, b #)) -- ^ Function to be applied atomically to the element
  -> State# d -- ^ Starting state
  -> (# State# d, b #)
atomicModifyIntArray# mba# i# f s0# =
  let go s# o# =
        case f o# of
          (# n#, artifact #) ->
            case casIntArray# mba# i# o# n# s# of
              (# s'#, o'# #) ->
                case o# ==# o'# of
                  0# -> go s# o'#
                  _  -> seq# artifact s'#
   in case atomicReadIntArray# mba# i# s0# of
        (# s'#, o# #) -> go s'# o#
{-# INLINE atomicModifyIntArray# #-}



-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier.
--
-- @since 0.1.0
atomicModifyIntPVar ::
     MonadPrim s m => PVar Int s -> (Int -> (Int, a)) -> m a
atomicModifyIntPVar (PVar mba#) f = primitive (atomicModifyIntArray# mba# 0# g)
  where
    g i# =
      case f (I# i#) of
        (I# o#, a) -> (# o#, a #)
    {-# INLINE g #-}
{-# INLINE atomicModifyIntPVar #-}


-- | Uses `casIntArray#` to perform atomic modification of an integer element in a
-- `MutableByteArray#`. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyIntArray_# ::
     MutableByteArray# d -- ^ Array to be mutated
  -> Int# -- ^ Index in number of `Int#` elements into the `MutableByteArray#`
  -> (Int# -> Int#) -- ^ Function to be applied atomically to the element
  -> State# d -- ^ Starting state
  -> State# d
atomicModifyIntArray_# mba# i# f s0# =
  let go s# o# =
        case casIntArray# mba# i# o# (f o#) s# of
          (# s'#, o'# #) ->
            case o# ==# o'# of
              0# -> go s# o'#
              _  -> s'#
   in case atomicReadIntArray# mba# i# s0# of
        (# s'#, o# #) -> go s'# o#
{-# INLINE atomicModifyIntArray_# #-}


-- | Apply a function to an integer element of a `PVar` atomically. Returns the old
-- value. Implies a full memory barrier.
--
-- @since 0.1.0
atomicModifyIntPVar_ ::
     MonadPrim s m => PVar Int s -> (Int -> Int) -> m ()
atomicModifyIntPVar_ (PVar mba#) f =
  primitive_ (atomicModifyIntArray_# mba# 0# (\i# -> unI# (f (I# i#))))
{-# INLINE atomicModifyIntPVar_ #-}


-- ghc-8.2 (i.e. 802 version) introduced these two functions, for versions before those
-- use their reimplementations in C:
# if __GLASGOW_HASKELL__ < 802
foreign import ccall unsafe "pvar.c pvar_is_byte_array_pinned"
  isByteArrayPinned# :: ByteArray# -> Int#
foreign import ccall unsafe "pvar.c pvar_is_byte_array_pinned"
  isMutableByteArrayPinned# :: MutableByteArray# s -> Int#
#endif
