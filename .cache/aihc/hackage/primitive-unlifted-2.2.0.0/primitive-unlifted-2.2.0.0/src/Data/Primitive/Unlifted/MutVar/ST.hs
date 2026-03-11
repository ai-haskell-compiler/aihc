{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}

module Data.Primitive.Unlifted.MutVar.ST
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
import Data.Primitive.Unlifted.MutVar.Primops
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import GHC.ST (ST (..))
import GHC.Exts (isTrue#, State#)

data UnliftedMutVar_ s a unlifted_a = UnliftedMutVar (UnliftedMutVar# s unlifted_a)
type role UnliftedMutVar_ nominal phantom representational

type UnliftedMutVar s a = UnliftedMutVar_ s a (Unlifted a)

instance (unlifted_a ~ Unlifted a) => PrimUnlifted (UnliftedMutVar_ s a unlifted_a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (UnliftedMutVar_ s a unlifted_a) = UnliftedMutVar# s unlifted_a
  toUnlifted# (UnliftedMutVar m) = m
  fromUnlifted# m = UnliftedMutVar m

instance (unlifted_a ~ Unlifted a) => Eq (UnliftedMutVar_ s a unlifted_a) where
  {-# INLINE (==) #-}
  UnliftedMutVar m1 == UnliftedMutVar m2
    = isTrue# (sameUnliftedMutVar# m1 m2)

primitive_ :: (State# s -> State# s) -> ST s ()
{-# INLINE primitive_ #-}
primitive_ f = ST $ \s -> (# f s, () #)

newUnliftedMutVar
  :: PrimUnlifted a
  => a -> ST s (UnliftedMutVar s a)
{-# INLINE newUnliftedMutVar #-}
newUnliftedMutVar a
  = ST $ \s -> case newUnliftedMutVar# (toUnlifted# a) s of
      (# s', mv #) -> (# s', UnliftedMutVar mv #)

readUnliftedMutVar
  :: PrimUnlifted a
  => UnliftedMutVar s a -> ST s a
{-# INLINE readUnliftedMutVar #-}
readUnliftedMutVar (UnliftedMutVar mv)
  = ST $ \s -> case readUnliftedMutVar# mv s of
      (# s', a #) -> (# s', fromUnlifted# a #)

writeUnliftedMutVar
  :: PrimUnlifted a
  => UnliftedMutVar s a -> a -> ST s ()
{-# INLINE writeUnliftedMutVar #-}
writeUnliftedMutVar (UnliftedMutVar mv) a
  = primitive_ $ writeUnliftedMutVar# mv (toUnlifted# a)

modifyUnliftedMutVar
  :: PrimUnlifted a
  => UnliftedMutVar s a -> (a -> a) -> ST s ()
{-# INLINE modifyUnliftedMutVar #-}
modifyUnliftedMutVar mv f = do
  a <- readUnliftedMutVar mv
  writeUnliftedMutVar mv (f a)

modifyUnliftedMutVar'
  :: PrimUnlifted a
  => UnliftedMutVar s a -> (a -> a) -> ST s ()
{-# INLINE modifyUnliftedMutVar' #-}
modifyUnliftedMutVar' mv f = do
  a <- readUnliftedMutVar mv
  writeUnliftedMutVar mv $! f a

casUnliftedMutVar
  :: PrimUnlifted a
  => UnliftedMutVar s a   -- ^ The 'UnliftedMutVar' on which to operate
  -> a -- ^ The expected value
  -> a -- ^ The new value to install if the 'UnliftedMutVar contains the expected value
  -> ST s (Bool, a)
{-# INLINE casUnliftedMutVar #-}
casUnliftedMutVar (UnliftedMutVar mv) old new = ST $ \s ->
  case casUnliftedMutVar# mv (toUnlifted# old) (toUnlifted# new) s of
    (# s', 0#, latest #) -> (# s', (False, fromUnlifted# latest) #)
    (# s', _, latest #) -> (# s', (True, fromUnlifted# latest) #)

atomicSwapUnliftedMutVar
  :: PrimUnlifted a
  => UnliftedMutVar s a
  -> a
  -> ST s a
{-# INLINE atomicSwapUnliftedMutVar #-}
atomicSwapUnliftedMutVar (UnliftedMutVar mv) a
  = ST $ \s -> case atomicSwapUnliftedMutVar# mv (toUnlifted# a) s of
      (# s', old #) -> (# s', fromUnlifted# old #)
