{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif
-- |
-- Module      : Data.Primitive.PVar
-- Copyright   : (c) Alexey Kuleshevich 2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Primitive.PVar
  ( -- | `PVar` has significantly better performance characteristics over
    -- `Data.IORef.IORef`, `Data.STRef.STRef` and `Data.Primitive.MutVar.MutVar`. This is
    -- because value is mutated directly in memory instead of following an extra
    -- pointer. Besides better performance there is another consequence of direct
    -- mutation, namely the value is always evaluated to normal form when being written
    -- into a `PVar`

    PVar
  , RW
  -- * Creation
  , newPVar
  , withPVarST
  -- * Mutable Operations
  , readPVar
  , writePVar
  , modifyPVar
  , modifyPVar_
  , fetchModifyPVar
  , modifyFetchPVar
  , modifyPVarM
  , modifyPVarM_
  , fetchModifyPVarM
  , modifyFetchPVarM
  , swapPVars_
  , swapPVars
  , copyPVar
  , sizeOfPVar
  , alignmentPVar
  -- * Pinned memory
  --
  -- $pinned
  , newPinnedPVar
  , newAlignedPinnedPVar
  , withPtrPVar
  , withStorablePVar
  , withAlignedStorablePVar
  , copyPVarToPtr
  , toForeignPtrPVar
  , isPinnedPVar
  , peekPrim
  , pokePrim
  -- -- * Numeric infix operations
  -- , (=+)
  -- , (=-)
  -- , (=*)
  -- , (=/)
  -- , (=%)
  -- * Atomic operations
  , atomicModifyIntPVar
  , atomicModifyIntPVar_
  , atomicFetchModifyIntPVar
  , atomicModifyFetchIntPVar
  , atomicReadIntPVar
  , atomicWriteIntPVar
  , casIntPVar
  , atomicAddIntPVar
  , atomicSubIntPVar
  , atomicAndIntPVar
  , atomicNandIntPVar
  , atomicOrIntPVar
  , atomicXorIntPVar
  , atomicNotIntPVar
  -- * Re-exports
  , Prim
  , MonadPrim
  , PrimMonad(PrimState)
  , RealWorld
  , sizeOf
  , alignment
  , ST
  , runST
  , S.Storable(peek, poke)
  ) where

import Control.Monad (void)
import Control.Monad.Primitive (MonadPrim, PrimMonad(primitive), PrimState, primitive_,
                                touch)
import Control.Monad.ST (ST, runST)
import Data.Primitive.PVar.Internal
import Data.Primitive.PVar.Unsafe
import Data.Primitive.Types
import qualified Foreign.Storable as S
import GHC.Exts
import GHC.ForeignPtr

-- $pinned
-- In theory it is unsafe to mix `S.Storable` and `Prim` operations on the same chunk of
-- memory, because some instances can have different memory layouts for the same
-- type. This is highly uncommon in practice and if you are intermixing the two concepts
-- together you probably already know what you are doing.

-- | Synonym for `RealWorld`
type RW = RealWorld

-- | Run an `ST` action on a mutable variable.
--
-- @since 0.1.0
withPVarST ::
     Prim p
  => p -- ^ Initial value assigned to the mutable variable
  -> (forall s. PVar p s -> ST s a) -- ^ Action to run
  -> a -- ^ Result produced by the `ST` action
withPVarST x st = runST (newPVar x >>= st)
{-# INLINE withPVarST #-}

-- | Apply an action to the `Ptr` that references the mutable variable, but only if it is
-- backed by pinned memory, cause otherwise it would be unsafe.
--
-- @since 0.1.0
withPtrPVar :: (MonadPrim s m, Prim a) => PVar a n -> (Ptr a -> m b) -> m (Maybe b)
withPtrPVar pvar f =
  case toPtrPVar pvar of
    Nothing -> return Nothing
    Just ptr -> do
      r <- f ptr
      touch pvar
      return $ Just r
{-# INLINE withPtrPVar #-}

-- | Convert `PVar` into a `ForeignPtr`, but only if it is backed by pinned memory.
--
-- @since 0.1.0
toForeignPtrPVar :: PVar a s -> Maybe (ForeignPtr a)
toForeignPtrPVar pvar
  | isPinnedPVar pvar = Just $ unsafeToForeignPtrPVar pvar
  | otherwise = Nothing
{-# INLINE toForeignPtrPVar #-}

-- | Copy contents of one mutable variable `PVar` into another
--
-- @since 0.1.0
copyPVar ::
     (MonadPrim s m, Prim a)
  => PVar a s -- ^ Source variable
  -> PVar a s -- ^ Destination variable
  -> m ()
copyPVar pvar@(PVar mbas#) (PVar mbad#) =
  primitive_ (copyMutableByteArray# mbas# 0# mbad# 0# (sizeOfPVar# pvar))
{-# INLINE copyPVar #-}

-- | Copy contents of a mutable variable `PVar` into a pointer `Ptr`
--
-- @since 0.1.0
copyPVarToPtr :: (MonadPrim s m, Prim a) => PVar a s -> Ptr a -> m ()
copyPVarToPtr pvar@(PVar mbas#) (Ptr addr#) =
  primitive_ (copyMutableByteArrayToAddr# mbas# 0# addr# (sizeOfPVar# pvar))
{-# INLINE copyPVarToPtr #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyPVar :: (MonadPrim s m, Prim a) => PVar a s -> (a -> (a, b)) -> m b
modifyPVar pvar f = modifyPVarM pvar (return . f)
{-# INLINE modifyPVar #-}

-- | Apply a pure function to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVar_ :: (MonadPrim s m, Prim a) => PVar a s -> (a -> a) -> m ()
modifyPVar_ pvar f = modifyPVarM_ pvar (return . f)
{-# INLINE modifyPVar_ #-}


-- | Apply a pure function to the contents of a mutable variable. Returns the old value.
--
-- @since 0.2.0
fetchModifyPVar :: (MonadPrim s m, Prim a) => PVar a s -> (a -> a) -> m a
fetchModifyPVar pvar f = fetchModifyPVarM pvar (return . f)
{-# INLINE fetchModifyPVar #-}

-- | Apply a pure function to the contents of a mutable variable. Returns the new value.
--
-- @since 0.2.0
modifyFetchPVar :: (MonadPrim s m, Prim a) => PVar a s -> (a -> a) -> m a
modifyFetchPVar pvar f = modifyFetchPVarM pvar (return . f)
{-# INLINE modifyFetchPVar #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the artifact of
-- computation.
--
-- @since 0.2.0
modifyPVarM :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m (a, b)) -> m b
modifyPVarM pvar f = do
  a <- readPVar pvar
  (a', b) <- f a
  b <$ writePVar pvar a'
{-# INLINE modifyPVarM #-}

-- | Apply a monadic action to the contents of a mutable variable. Returns the old value.
--
-- @since 0.2.0
fetchModifyPVarM :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m a) -> m a
fetchModifyPVarM pvar f = do
  a <- readPVar pvar
  a <$ (writePVar pvar =<< f a)
{-# INLINE fetchModifyPVarM #-}


-- | Apply a monadic action to the contents of a mutable variable. Returns the new value.
--
-- @since 0.2.0
modifyFetchPVarM :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m a) -> m a
modifyFetchPVarM pvar f = do
  a <- readPVar pvar
  a' <- f a
  a' <$ writePVar pvar a'
{-# INLINE modifyFetchPVarM #-}


-- | Apply a monadic action to the contents of a mutable variable.
--
-- @since 0.1.0
modifyPVarM_ :: (MonadPrim s m, Prim a) => PVar a s -> (a -> m a) -> m ()
modifyPVarM_ pvar f = readPVar pvar >>= f >>= writePVar pvar
{-# INLINE modifyPVarM_ #-}

-- | Swap contents of two mutable variables. Returns their old values.
--
-- @since 0.1.0
swapPVars :: (MonadPrim s m, Prim a) => PVar a s -> PVar a s -> m (a, a)
swapPVars pvar1 pvar2 = do
  a1 <- readPVar pvar1
  a2 <- fetchModifyPVar pvar2 (const a1)
  (a1, a2) <$ writePVar pvar1 a2
{-# INLINE swapPVars #-}

-- | Swap contents of two mutable variables.
--
-- @since 0.1.0
swapPVars_ :: (MonadPrim s m, Prim a) => PVar a s -> PVar a s -> m ()
swapPVars_ pvar1 pvar2 = void $ swapPVars pvar1 pvar2
{-# INLINE swapPVars_ #-}

-- TODO: Come up with a concrete interface for numerics
-- (=+) :: (MonadPrim s m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=+) pvar a = modifyPVar_ pvar (+ a)
-- {-# INLINE (=+) #-}

-- (=-) :: (MonadPrim s m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=-) pvar a = modifyPVar_ pvar (subtract a)
-- {-# INLINE (=-) #-}

-- (=*) :: (MonadPrim s m, Prim a, Num a) => PVar (PrimState m) a -> a -> m ()
-- (=*) pvar a = modifyPVar_ pvar (* a)
-- {-# INLINE (=*) #-}

-- (=/) :: (MonadPrim s m, Prim a, Fractional a) => PVar (PrimState m) a -> a -> m ()
-- (=/) pvar a = modifyPVar_ pvar (/ a)
-- {-# INLINE (=/) #-}

-- -- | C like modulo operator
-- (=%) :: (MonadPrim s m, Prim a, Integral a) => PVar (PrimState m) a -> a -> m ()
-- (=%) pvar a = modifyPVar_ pvar (`mod` a)
-- {-# INLINE (=%) #-}




-- | Apply an action to the newly allocated `PVar` and to the `Ptr` that references
-- it. Memory allocated with number of bytes specified by @`S.sizeOf` a@ is allocated and
-- pinned, therefore it is safe to operate directly with the pointer as well as over
-- FFI. Returning the pointer from the supplied action would be very unsafe, therefore
-- return the `PVar` if you still need it afterwards, garbage collector will cleanup the
-- memory when it is no longer needed.
--
-- @since 0.1.0
withStorablePVar ::
     (MonadPrim s m, S.Storable a)
  => a -- ^ Initial value
  -> (PVar a s -> Ptr a -> m b) -- ^ Action to run
  -> m b
withStorablePVar a f = do
  pvar <- rawStorablePVar
  runWithPokedPtr pvar a f
{-# INLINE withStorablePVar #-}

-- | Same `withStorablePVar`, except memory is aligned according to `S.alignment`.
--
-- @since 0.1.0
withAlignedStorablePVar ::
     (MonadPrim s m, S.Storable a)
  => a -- ^ Initial value
  -> (PVar a s -> Ptr a -> m b) -- ^ Action to run
  -> m b
withAlignedStorablePVar a f = do
  pvar <- rawAlignedStorablePVar
  runWithPokedPtr pvar a f
{-# INLINE withAlignedStorablePVar #-}


-- | Read a value from `PVar` atomically. Implies a full memory barrier.
--
-- @since 0.1.0
atomicReadIntPVar :: MonadPrim s m => PVar Int s -> m Int
atomicReadIntPVar (PVar mba#) =
  primitive $ \s# ->
    case atomicReadIntArray# mba# 0# s# of
      (# s'#, i# #) -> (# s'#, I# i# #)
{-# INLINE atomicReadIntPVar #-}

-- | Write a value into an `PVar` atomically. Implies a full memory barrier.
--
-- @since 0.1.0
atomicWriteIntPVar :: MonadPrim s m => PVar Int s -> Int -> m ()
atomicWriteIntPVar (PVar mba#) a = primitive_ (atomicWriteIntArray# mba# 0# (unI# a))
{-# INLINE atomicWriteIntPVar #-}


-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier. Returns the new value.
--
-- @since 0.2.0
atomicFetchModifyIntPVar ::
     MonadPrim s m => PVar Int s -> (Int -> Int) -> m Int
atomicFetchModifyIntPVar pvar f =
  atomicModifyIntPVar pvar $ \a ->
    let a' = f a
     in a' `seq` (a', a)
{-# INLINE atomicFetchModifyIntPVar #-}

-- | Apply a function to an integer element of a `PVar` atomically. Implies a full memory
-- barrier. Returns the new value.
--
-- @since 0.2.0
atomicModifyFetchIntPVar ::
     MonadPrim s m => PVar Int s -> (Int -> Int) -> m Int
atomicModifyFetchIntPVar pvar f =
  atomicModifyIntPVar pvar $ \a ->
    let a' = f a
     in a' `seq` (a', a')
{-# INLINE atomicModifyFetchIntPVar #-}


-- | Compare and swap. This is also a function that is used to implement
-- `atomicModifyIntPVar`. Implies a full memory barrier.
--
-- @since 0.1.0
casIntPVar ::
     MonadPrim s m
  => PVar Int s -- ^ Variable to mutate
  -> Int -- ^ Old expected value
  -> Int -- ^ New value
  -> m Int -- ^ Old actual value
casIntPVar (PVar mba#) old new =
  primitive $ \s# ->
    case casIntArray# mba# 0# (unI# old) (unI# new) s# of
      (# s'#, i'# #) -> (# s'#, I# i'# #)
{-# INLINE casIntPVar #-}



-- | Add two numbers, corresponds to @(`+`)@ done atomically. Returns the previous value of
-- the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicAddIntPVar :: MonadPrim s m => PVar Int s -> Int -> m Int
atomicAddIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchAddIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicAddIntPVar #-}

-- | Subtract two numbers, corresponds to @(`-`)@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicSubIntPVar :: MonadPrim s m => PVar Int s -> Int -> m Int
atomicSubIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchSubIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicSubIntPVar #-}


-- | Binary conjuction (AND), corresponds to @(`Data.Bits..&.`)@ done atomically. Returns the previous
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicAndIntPVar :: MonadPrim s m => PVar Int s -> Int -> m Int
atomicAndIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchAndIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicAndIntPVar #-}


-- | Binary negation of conjuction (NAND), corresponds to @\\x y -> `Data.Bits.complement` (x
-- `Data.Bits..&.` y)@ done atomically. Returns the previous value of the mutable variable. Implies
-- a full memory barrier.
--
-- @since 0.1.0
atomicNandIntPVar :: MonadPrim s m => PVar Int s -> Int -> m Int
atomicNandIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchNandIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicNandIntPVar #-}


-- | Binary disjunction (OR), corresponds to @(`Data.Bits..|.`)@ done atomically. Returns the previous
-- value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicOrIntPVar :: MonadPrim s m => PVar Int s -> Int -> m Int
atomicOrIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchOrIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicOrIntPVar #-}


-- | Binary exclusive disjunction (XOR), corresponds to @`Data.Bits.xor`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicXorIntPVar :: MonadPrim s m => PVar Int s -> Int -> m Int
atomicXorIntPVar (PVar mba#) a =
  primitive $ \s# ->
    case fetchXorIntArray# mba# 0# (unI# a) s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
{-# INLINE atomicXorIntPVar #-}


-- | Binary negation (NOT), corresponds to ones' @`Data.Bits.complement`@ done atomically. Returns the
-- previous value of the mutable variable. Implies a full memory barrier.
--
-- @since 0.1.0
atomicNotIntPVar :: MonadPrim s m => PVar Int s -> m Int
atomicNotIntPVar (PVar mba#) =
  primitive $ \s# ->
    case fetchXorIntArray# mba# 0# fullInt# s# of
      (# s'#, p# #) -> (# s'#, I# p# #)
  where
    fullInt# =
      case maxBound :: Word of
        W# w# -> word2Int# w#
{-# INLINE atomicNotIntPVar #-}

