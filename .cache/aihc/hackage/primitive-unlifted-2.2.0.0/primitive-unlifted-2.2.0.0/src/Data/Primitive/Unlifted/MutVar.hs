{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}

module Data.Primitive.Unlifted.MutVar
  ( UnliftedMutVar_ (..)
  , UnliftedMutVar
  , newUnliftedMutVar
  , readUnliftedMutVar
  , writeUnliftedMutVar
  , modifyUnliftedMutVar
  , modifyUnliftedMutVar'
  , casUnliftedMutVar
  , atomicSwapUnliftedMutVar
  ) where
import Data.Primitive.Unlifted.Class (PrimUnlifted)
import Control.Monad.Primitive (PrimMonad (PrimState), stToPrim)
import qualified Data.Primitive.Unlifted.MutVar.ST as M
import Data.Primitive.Unlifted.MutVar.ST (UnliftedMutVar_ (..), UnliftedMutVar)

newUnliftedMutVar
  :: (PrimMonad m, PrimUnlifted a)
  => a -> m (UnliftedMutVar (PrimState m) a)
{-# INLINE newUnliftedMutVar #-}
newUnliftedMutVar a = stToPrim $ M.newUnliftedMutVar a

readUnliftedMutVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMutVar (PrimState m) a -> m a
{-# INLINE readUnliftedMutVar #-}
readUnliftedMutVar mv = stToPrim $ M.readUnliftedMutVar mv

writeUnliftedMutVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMutVar (PrimState m) a -> a -> m ()
{-# INLINE writeUnliftedMutVar #-}
writeUnliftedMutVar mv a = stToPrim $ M.writeUnliftedMutVar mv a

modifyUnliftedMutVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMutVar (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyUnliftedMutVar #-}
modifyUnliftedMutVar mv f = stToPrim $ M.modifyUnliftedMutVar mv f

modifyUnliftedMutVar'
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMutVar (PrimState m) a -> (a -> a) -> m ()
{-# INLINE modifyUnliftedMutVar' #-}
modifyUnliftedMutVar' mv f = stToPrim $ M.modifyUnliftedMutVar' mv f

casUnliftedMutVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMutVar (PrimState m) a   -- ^ The 'UnliftedMutVar' on which to operate
  -> a -- ^ The expected value
  -> a -- ^ The new value to install if the 'UnliftedMutVar contains the expected value
  -> m (Bool, a)
{-# INLINE casUnliftedMutVar #-}
casUnliftedMutVar mv old new = stToPrim $ M.casUnliftedMutVar mv old new

atomicSwapUnliftedMutVar
  :: (PrimMonad m, PrimUnlifted a)
  => UnliftedMutVar (PrimState m) a
  -> a
  -> m a
{-# INLINE atomicSwapUnliftedMutVar #-}
atomicSwapUnliftedMutVar mv a = stToPrim $ M.atomicSwapUnliftedMutVar mv a
