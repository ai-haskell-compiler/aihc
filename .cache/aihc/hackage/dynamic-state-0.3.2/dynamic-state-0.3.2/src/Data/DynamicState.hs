{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.DynamicState
-- License     :  GPL2
-- Maintainer  :  zcarterc@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements a simple HashMap ConcreteTypeRep Dynamic

module Data.DynamicState (
  DynamicState(..),
  getDyn,
  setDyn,
  _dyn
  ) where

import Data.Dynamic
import Data.HashMap.Strict as M
import Data.ConcreteTypeRep

-- | An extensible record, indexed by type
newtype DynamicState = DynamicState { unDynamicState :: M.HashMap ConcreteTypeRep Dynamic }
  deriving (Typeable)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup DynamicState where
  DynamicState a <> DynamicState b = DynamicState (a <> b)
#endif

instance Monoid DynamicState where
#if !MIN_VERSION_base(4,11,0)
  mappend (DynamicState a) (DynamicState b) = DynamicState (mappend a b)
#endif
  mempty = DynamicState mempty

getDyn :: forall a. Typeable a => DynamicState -> Maybe a
getDyn (DynamicState ds) = M.lookup (cTypeOf (undefined :: a)) ds >>= fromDynamic

setDyn :: forall a. Typeable a => DynamicState -> a -> DynamicState
setDyn (DynamicState ds) x = DynamicState $ M.insert (cTypeOf (undefined :: a)) (toDyn x) ds

-- | Lens with default value
_dyn :: (Typeable a, Functor f) => a -> (a -> f a) -> DynamicState -> f DynamicState
_dyn def afb s = setDyn s <$> afb (maybe def id $ getDyn s)
