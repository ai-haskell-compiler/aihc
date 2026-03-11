{-# language UnboxedTuples #-}
{-# language UnboxedSums #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language RankNTypes #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language BangPatterns #-}
{- options_ghc -ddump-simpl #-}

-- | This module includes all the features of "Control.Concurrent.MVar", except
-- that the functions in "Data.Primitive.Unlifted.Weak" subsume the functionality
-- of @mkWeakMV@ and @addMVarFinalizer@, so we do not include analogues of those
-- functions.
module Data.Primitive.Unlifted.MVar
  ( UnliftedMVar_ (..)
  , UnliftedMVar
  , newUnliftedMVar
  , newEmptyUnliftedMVar
  , takeUnliftedMVar
  , tryTakeUnliftedMVar
  , putUnliftedMVar
  , tryPutUnliftedMVar
  , readUnliftedMVar
  , tryReadUnliftedMVar
  , isEmptyUnliftedMVar
  , swapUnliftedMVar
  , withUnliftedMVar
  , withUnliftedMVarMasked
  , modifyUnliftedMVar
  , modifyUnliftedMVar_
  , modifyUnliftedMVarMasked
  , modifyUnliftedMVarMasked_
  ) where
import qualified Data.Primitive.Unlifted.MVar.ST as MV
import Data.Primitive.Unlifted.MVar.ST
         ( UnliftedMVar_ (..), type UnliftedMVar )
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import GHC.Exts (RealWorld)
import Control.Monad.Primitive (stToPrim, PrimMonad (..), PrimBase, primToST)

newUnliftedMVar
  :: (PrimUnlifted a, PrimMonad m)
  => a -> m (UnliftedMVar (PrimState m) a)
newUnliftedMVar a = stToPrim $ MV.newUnliftedMVar a

newEmptyUnliftedMVar
  :: PrimMonad m
  => m (UnliftedMVar (PrimState m) a)
{-# INLINE newEmptyUnliftedMVar #-}
newEmptyUnliftedMVar = stToPrim $ MV.newEmptyUnliftedMVar

takeUnliftedMVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMVar (PrimState m) a -> m a
{-# INLINE takeUnliftedMVar #-}
takeUnliftedMVar mv = stToPrim $ MV.takeUnliftedMVar mv

tryTakeUnliftedMVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMVar (PrimState m) a -> m (Maybe a)
{-# INLINE tryTakeUnliftedMVar #-}
tryTakeUnliftedMVar mv = stToPrim $ MV.tryTakeUnliftedMVar mv

putUnliftedMVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMVar (PrimState m) a -> a -> m ()
{-# INLINE putUnliftedMVar #-}
putUnliftedMVar mv a = stToPrim $ MV.putUnliftedMVar mv a

tryPutUnliftedMVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMVar (PrimState m) a -> a -> m Bool
{-# INLINE tryPutUnliftedMVar #-}
tryPutUnliftedMVar mv a = stToPrim $ MV.tryPutUnliftedMVar mv a

readUnliftedMVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMVar (PrimState m) a -> m a
{-# INLINE readUnliftedMVar #-}
readUnliftedMVar mv = stToPrim $ MV.readUnliftedMVar mv

tryReadUnliftedMVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMVar (PrimState m) a -> m (Maybe a)
{-# INLINE tryReadUnliftedMVar #-}
tryReadUnliftedMVar mv = stToPrim $ MV.tryReadUnliftedMVar mv

isEmptyUnliftedMVar
  :: PrimMonad m
  => UnliftedMVar (PrimState m) a -> m Bool
{-# INLINE isEmptyUnliftedMVar #-}
isEmptyUnliftedMVar mv = stToPrim $ MV.isEmptyUnliftedMVar mv

swapUnliftedMVar
  :: (PrimMonad m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> a -> m a
{-# INLINE swapUnliftedMVar #-}
swapUnliftedMVar mvar new = stToPrim $ MV.swapUnliftedMVar mvar new

withUnliftedMVar
  :: (PrimBase m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> (a -> m b) -> m b
{-# INLINE withUnliftedMVar #-}
withUnliftedMVar m f = stToPrim $ MV.withUnliftedMVar m (primToST . f)

withUnliftedMVarMasked
  :: (PrimBase m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> (a -> m b) -> m b
{-# INLINE withUnliftedMVarMasked #-}
withUnliftedMVarMasked m st = stToPrim $ MV.withUnliftedMVarMasked m (primToST . st)

modifyUnliftedMVar
  :: (PrimBase m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> (a -> m (a, b)) -> m b
{-# INLINE modifyUnliftedMVar #-}
modifyUnliftedMVar m st = stToPrim $ MV.modifyUnliftedMVar m (primToST . st)

modifyUnliftedMVar_
  :: (PrimBase m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> (a -> m a) -> m ()
{-# INLINE modifyUnliftedMVar_ #-}
modifyUnliftedMVar_ m st = stToPrim $ MV.modifyUnliftedMVar_ m (primToST . st)

modifyUnliftedMVarMasked
  :: (PrimBase m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> (a -> m (a, b)) -> m b
{-# INLINE modifyUnliftedMVarMasked #-}
modifyUnliftedMVarMasked m st = stToPrim $ MV.modifyUnliftedMVarMasked m (primToST . st)

modifyUnliftedMVarMasked_
  :: (PrimBase m, PrimState m ~ RealWorld, PrimUnlifted a)
  => UnliftedMVar RealWorld a -> (a -> m a) -> m ()
{-# INLINE modifyUnliftedMVarMasked_ #-}
modifyUnliftedMVarMasked_ m st = stToPrim $ MV.modifyUnliftedMVarMasked_ m (primToST . st)
