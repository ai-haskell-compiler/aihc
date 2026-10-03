{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Public where

import Data.Kind
import Data.Proxy
import Data.Typeable
import Natural

newtype Tannen f p a b = Tannen {runTannen :: f (p a b)}

instance Functor f => BifunctorFunctor (Tannen f) where
  bifmap f (Tannen p) = Tannen (fmap f p)

data Index = Low | High

data Pair (a :: Index) (b :: Index) = Pair

identityMap :: Functor f => Tannen f Pair 'Low 'High -> Tannen f Pair 'Low 'High
identityMap = bifmap (\x -> x)

newtype Tagged s b = Tagged b

partialRep :: forall (s :: Type). Typeable s => Proxy s -> TypeRep
partialRep _ = typeRep (Proxy :: Proxy (Tagged s))

data Product f g a b = Product (f a b) (g a b)

deriving instance (Functor (f a), Functor (g a)) => Functor (Product f g a)
deriving instance (Foldable (f a), Foldable (g a)) => Foldable (Product f g a)
deriving instance (Traversable (f a), Traversable (g a)) => Traversable (Product f g a)
