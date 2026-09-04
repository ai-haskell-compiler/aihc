{-# LANGUAGE KindSignatures #-}

module Data.Traversable
  ( Traversable (..),
    for,
    forM,
  )
where

import Data.Foldable (Foldable)
import Data.Kind (Type)
import Prelude
  ( Applicative (..),
    Either (..),
    Functor (..),
    Maybe (..),
    Monad,
    id,
    (.),
  )

class (Functor t, Foldable t) => Traversable (t :: Type -> Type) where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequenceA :: (Applicative f) => t (f a) -> f (t a)
  mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
  sequence :: (Monad m) => t (m a) -> m (t a)

  traverse f = sequenceA . fmap f
  sequenceA = traverse id
  mapM = traverse
  sequence = sequenceA

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just value) = fmap Just (f value)

instance Traversable [] where
  traverse _ [] = pure []
  traverse f (value : values) = fmap (:) (f value) <*> traverse f values

instance Traversable (Either e) where
  traverse _ (Left value) = pure (Left value)
  traverse f (Right value) = fmap Right (f value)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for structure f = traverse f structure

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM structure f = mapM f structure
