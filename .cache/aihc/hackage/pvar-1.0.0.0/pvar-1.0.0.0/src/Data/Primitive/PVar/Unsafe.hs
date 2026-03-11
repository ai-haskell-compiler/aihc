{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Primitive.PVar.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar.Unsafe
  ( PVar(..)
  -- * Creation
  , rawPVar
  , rawPinnedPVar
  , rawAlignedPinnedPVar
  , rawStorablePVar
  , rawAlignedStorablePVar
  -- * Access
  , peekPrim
  , pokePrim
  -- * Conversion
  , toPtrPVar
  , unsafeToPtrPVar
  , unsafeToForeignPtrPVar
  -- * Reset
  , zeroPVar
  -- * Unpacked opartions
  , sizeOfPVar#
  , alignmentPVar#
  , setPVar#
  -- * ByteArray
  -- ** Atomic operations
  , atomicModifyIntArray#
  , atomicModifyIntArray_#
  -- ** Memory copying
  , copyFromByteArrayPVar
  , copyFromMutableByteArrayPVar
  , copyPVarToMutableByteArray
  -- ** Check if memory is pinned
  , isByteArrayPinned
  , isMutableByteArrayPinned
  -- *** Primitive versions
  , isByteArrayPinned#
  , isMutableByteArrayPinned#
  -- * Helpers
  , showsType
  , unI#
  )
  where

import Control.Monad.Primitive (MonadPrim, primitive_)
import Data.Primitive.PVar.Internal
import Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..))
import Data.Primitive.Types
import GHC.Exts as Exts
import GHC.ForeignPtr
import Data.Typeable


-- | Convert `PVar` into a `ForeignPtr`, very unsafe if not backed by pinned memory.
--
-- @since 0.1.0
unsafeToForeignPtrPVar :: PVar a s -> ForeignPtr a
unsafeToForeignPtrPVar pvar@(PVar mba#) =
  case unsafeToPtrPVar pvar of
    Ptr addr# -> ForeignPtr addr# (PlainPtr (unsafeCoerce# mba#))
{-# INLINE unsafeToForeignPtrPVar #-}



-- | Extract the address to the mutable variable, but only if it is backed by pinned
-- memory. It is unsafe because even for pinned memory memory can be deallocated if
-- associated `PVar` goes out of scope. Use `Data.Primitive.PVar.withPtrPVar` or
-- `Data.Primitive.PVar.toForeignPtr` instead.
--
-- @since 0.1.0
toPtrPVar :: PVar a s -> Maybe (Ptr a)
toPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toPtrPVar #-}

-- | Fill the contents of mutable variable with byte @c@
--
-- @since 0.1.0
setPVar# ::
     (MonadPrim s m, Prim a)
  => PVar a s
  -> Int# -- ^ Byte value to fill the `PVar` with
  -> m ()
setPVar# pvar@(PVar mba#) a# =
  primitive_ (Exts.setByteArray# mba# 0# (sizeOfPVar# pvar) a#)
{-# INLINE setPVar# #-}

-- | Reset contents of a mutable variable to zero.
--
-- @since 0.1.0
zeroPVar :: (MonadPrim s m, Prim a) => PVar a s -> m ()
zeroPVar pvar = setPVar# pvar 0#
{-# INLINE zeroPVar #-}

-- | Copy the value from a mutable variable into a mutable array at the specified index. Index
-- of array is not checked and can result in an unchecked exception when incorrect
--
-- @since 0.1.0
copyPVarToMutableByteArray ::
     (MonadPrim s m, Prim a)
  => PVar a s
  -> MutableByteArray s
  -> Int -- ^ Offset in number of elements into the array
  -> m ()
copyPVarToMutableByteArray pvar mba offset =
  copyBytesPVarToMutableByteArray pvar mba (offset * sizeOfPVar pvar)
{-# INLINE copyPVarToMutableByteArray #-}


-- | Copy the value from a frozen `ByteArray` into a mutable variable at specified
-- index. Index of array is not checked and can result in an unchecked exception when
-- incorrect
--
-- @since 0.1.0
copyFromByteArrayPVar ::
     (MonadPrim s m, Prim a)
  => ByteArray -- ^ Source array
  -> Int -- ^ Offset in number of elements into the array
  -> PVar a s
  -> m ()
copyFromByteArrayPVar ba offset pvar =
  copyBytesFromByteArrayPVar ba (offset * sizeOfPVar pvar) pvar
{-# INLINE copyFromByteArrayPVar #-}

-- | Copy the value from MutableByteArray at specified index into the mutable
-- variable. Index of array is not checked and can result in an unchecked exception when
-- incorrect
--
-- @since 0.1.0
copyFromMutableByteArrayPVar ::
     (MonadPrim s m, Prim a)
  => MutableByteArray s
  -> Int -- ^ Offset in number of elements into the array
  -> PVar a s
  -> m ()
copyFromMutableByteArrayPVar mba offset pvar =
  copyBytesFromMutableByteArrayPVar mba (offset * sizeOfPVar pvar) pvar
{-# INLINE copyFromMutableByteArrayPVar #-}


-- | Copy the value from a mutable variable into a `MutableByteArray` at the specified
-- offset in number of bytes. Offset into the array is not checked and can result in an
-- unchecked exception when incorrect
--
-- @since 0.1.0
copyBytesPVarToMutableByteArray ::
     (MonadPrim s m, Prim a)
  => PVar a s
  -> MutableByteArray s
  -> Int -- ^ Offset in bytes into the array
  -> m ()
copyBytesPVarToMutableByteArray pvar@(PVar mbas#) (MutableByteArray mbad#) (I# offset#) =
  primitive_ (copyMutableByteArray# mbas# 0# mbad# offset# (sizeOfPVar# pvar))
{-# INLINE copyBytesPVarToMutableByteArray #-}


-- | Copy the value from a frozen `ByteArray` at the specified offset in number of bytes
-- into a mutable variable. Offset into the array is not checked and can result in an
-- unchecked exception when incorrect
--
-- @since 0.1.0
copyBytesFromByteArrayPVar ::
     (MonadPrim s m, Prim a)
  => ByteArray -- ^ Source array
  -> Int -- ^ Offset in bytes into the array
  -> PVar a s
  -> m ()
copyBytesFromByteArrayPVar (ByteArray ba#) (I# offset#) pvar@(PVar mba#) =
  primitive_ (copyByteArray# ba# offset# mba# 0# (sizeOfPVar# pvar))
{-# INLINE copyBytesFromByteArrayPVar #-}

-- | Copy the value from a `MutableByteArray` at an offset in bytes into the mutable
-- variable. Offset into the array is not checked and can result in an unchecked exception
-- when incorrect
--
-- @since 0.1.0
copyBytesFromMutableByteArrayPVar ::
     (MonadPrim s m, Prim a)
  => MutableByteArray s
  -> Int -- ^ Offset in bytes into the array
  -> PVar a s
  -> m ()
copyBytesFromMutableByteArrayPVar (MutableByteArray mbas#) (I# offset#) pvar@(PVar mbad#) =
  primitive_ (copyMutableByteArray# mbas# offset# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyBytesFromMutableByteArrayPVar #-}


-- | Show the type name
showsType :: Typeable t => proxy t -> ShowS
showsType = showsTypeRep . typeRep


-- | Check whether or not the `ByteArray` is pinned.
--
-- /__Note__/ - This function uses GHC built-in functions for GHC 8.2 and newer, but for older
-- versions it fallsback onto custom implementation.
--
--  @since 0.1.1
isByteArrayPinned :: ByteArray -> Bool
isByteArrayPinned (ByteArray arr#) = isTrue# (isByteArrayPinned# arr#)
{-# INLINE isByteArrayPinned #-}

-- | Check whether or not the `MutableByteArray` is pinned.
--
-- /__Note__/ - This function uses GHC built-in functions for GHC 8.2 and newer, but for older
-- versions it fallsback onto custom implementation.
--
--  @since 0.1.1
isMutableByteArrayPinned :: MutableByteArray s -> Bool
isMutableByteArrayPinned (MutableByteArray marr#) = isTrue# (isMutableByteArrayPinned# marr#)
{-# INLINE isMutableByteArrayPinned #-}
