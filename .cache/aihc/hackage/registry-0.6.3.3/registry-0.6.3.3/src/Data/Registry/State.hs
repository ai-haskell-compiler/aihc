{-# LANGUAGE AllowAmbiguousTypes #-}

-- | This module is experimental and is not added to the top level module Data.Registry.
--   It is not quite sure if we can / should support a useful state monad for passing a registry around
module Data.Registry.State where

import Control.Monad.Morph
import Data.Registry.Internal.Types
import Data.Registry.Lift
import Data.Registry.Registry
import Data.Registry.Solver
import Protolude

-- | Run some registry modifications in the StateT monad
runS :: (MFunctor m, Monad n) => Registry ins out -> m (StateT (Registry ins out) n) a -> m n a
runS r = hoist (`evalStateT` r)

-- | Add an element to the registry without changing its type
addFunTo :: forall m a b ins out. (ApplyVariadic m a b, Typeable a, Typeable b, IsSubset (Inputs b) out b) => a -> Registry ins out -> Registry ins out
addFunTo = addToRegistry @b . funTo @m

-- | Add an element to the registry without changing its type
--   *** This possibly adds untracked input types / output type! ***
addFunToUnsafe :: forall m a b ins out. (ApplyVariadic m a b, Typeable a, Typeable b) => a -> Registry ins out -> Registry ins out
addFunToUnsafe = addToRegistryUnsafe @b . funTo @m

-- | Add an element to the registry without changing its type, in the State monad
addFunS :: (Typeable a, IsSubset (Inputs a) out a, MonadState (Registry ins out) m) => a -> m ()
addFunS = modify . addFun

-- | Add an element to the registry without changing its type, in the State monad
--   *** This possibly adds untracked input types / output type! ***
addFunUnsafeS :: (Typeable a, MonadState (Registry ins out) m) => a -> m ()
addFunUnsafeS = modify . addFunUnsafe

-- | Add an element to the registry without changing its type, in the State monad
addToS :: forall n a b m ins out. (ApplyVariadic n a b, Typeable a, Typeable b, Typeable a, IsSubset (Inputs b) out b, MonadState (Registry ins out) m) => a -> m ()
addToS = modify . addFunTo @n @a @b

-- | Add an element to the registry without changing its type, in the State monad
--   *** This possibly adds untracked input types / output type! ***
addToUnsafeS :: forall n a b m ins out. (ApplyVariadic n a b, Typeable a, Typeable b, Typeable a, MonadState (Registry ins out) m) => a -> m ()
addToUnsafeS = modify . addFunToUnsafe @n @a @b

-- | Add an element to the registry without changing its type
addFun :: (Typeable a, IsSubset (Inputs a) out a) => a -> Registry ins out -> Registry ins out
addFun = addToRegistry . fun

-- | Add an element to the registry without changing its type
--   *** This possibly adds untracked input types / output type! ***
addFunUnsafe :: (Typeable a) => a -> Registry ins out -> Registry ins out
addFunUnsafe = addToRegistryUnsafe . fun

-- | Register modifications of elements which types are already in the registry
addToRegistry :: (Typeable a, IsSubset (Inputs a) out a) => Typed a -> Registry ins out -> Registry ins out
addToRegistry t (Registry entries specializations modifiers) =
  Registry (addEntry t entries) specializations modifiers

-- | Concatenate a registry to another statefully (to be used with $(makeGenerators ''MyType))
concatUnsafeS :: (MonadState (Registry ins out) m) => Registry ins' out' -> m ()
concatUnsafeS r = modify (concatRegistryUnsafe r)

-- | Register modifications of the registry without changing its type
addToRegistryUnsafe :: (Typeable a) => Typed a -> Registry ins out -> Registry ins out
addToRegistryUnsafe t (Registry entries specializations modifiers) =
  Registry (addEntry t entries) specializations modifiers

-- | Concatenate 2 registries
concatRegistryUnsafe :: Registry ins out -> Registry ins' out' -> Registry ins' out'
concatRegistryUnsafe
  (Registry fs1 ss1 ms1)
  (Registry fs2 ss2 ms2) = Registry (fs1 <> fs2) (ss1 <> ss2) (ms1 <> ms2)
