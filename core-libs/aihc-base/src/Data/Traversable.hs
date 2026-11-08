{-# LANGUAGE TupleSections #-}

module Data.Traversable
  ( Traversable (..),
    for,
    forM,
    mapAccumL,
    mapAccumR,
    mapAccumM,
    forAccumM,
    fmapDefault,
    foldMapDefault,
  )
where

import Control.Applicative (Const (..))
import Control.Monad (forM)
import Data.Monoid (Monoid)
import GHC.Internal.Traversable (Traversable (..))
import Prelude (Applicative (..), Functor (..), Monad (..), (.))

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for structure f = traverse f structure

newtype StateL s a = StateL {runStateL :: s -> (s, a)}

instance Functor (StateL s) where
  fmap f (StateL action) = StateL (\s -> let ~(next, value) = action s in (next, f value))

instance Applicative (StateL s) where
  pure value = StateL (,value)
  StateL function <*> StateL argument =
    StateL
      ( \s ->
          let ~(next, f) = function s
              ~(final, value) = argument next
           in (final, f value)
      )

newtype StateR s a = StateR {runStateR :: s -> (s, a)}

instance Functor (StateR s) where
  fmap f (StateR action) = StateR (\s -> let ~(next, value) = action s in (next, f value))

instance Applicative (StateR s) where
  pure value = StateR (,value)
  StateR function <*> StateR argument =
    StateR
      ( \s ->
          let ~(next, value) = argument s
              ~(final, f) = function next
           in (final, f value)
      )

mapAccumL :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumL f initial values = runStateL (traverse (\value -> StateL (`f` value)) values) initial

mapAccumR :: (Traversable t) => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
mapAccumR f initial values = runStateR (traverse (\value -> StateR (`f` value)) values) initial

newtype StateM s m a = StateM {runStateM :: s -> m (s, a)}

instance (Monad m) => Functor (StateM s m) where
  fmap f (StateM action) = StateM (\s -> action s >>= \(next, value) -> pure (next, f value))

instance (Monad m) => Applicative (StateM s m) where
  pure value = StateM (\s -> pure (s, value))
  StateM function <*> StateM argument =
    StateM
      ( \s ->
          function s >>= \(next, f) ->
            argument next >>= \(final, value) -> pure (final, f value)
      )

-- | A monadic 'mapAccumL'.
mapAccumM :: (Monad m, Traversable t) => (s -> a -> m (s, b)) -> s -> t a -> m (s, t b)
mapAccumM f initial values = runStateM (traverse (\value -> StateM (`f` value)) values) initial

-- | 'mapAccumM' with the arguments in a different order.
forAccumM :: (Monad m, Traversable t) => s -> t a -> (s -> a -> m (s, b)) -> m (s, t b)
forAccumM initial values f = mapAccumM f initial values

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f values = getConst (traverse (Const . f) values)

fmapDefault :: (Traversable t) => (a -> b) -> t a -> t b
fmapDefault f values =
  let (_, result) = mapAccumL (\state value -> (state, f value)) () values
   in result
