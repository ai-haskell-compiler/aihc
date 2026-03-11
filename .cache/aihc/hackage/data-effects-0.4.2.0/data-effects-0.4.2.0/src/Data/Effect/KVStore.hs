{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the t`KVStore` effect, comes
from [@Polysemy.KVStore@](https://hackage.haskell.org/package/polysemy-kvstore-0.1.3.0/docs/Polysemy-KVStore.html)
in the @polysemy-kvstore@ package.
-}
module Data.Effect.KVStore where

import Control.Arrow ((>>>))
import Control.Effect.Transform (raiseUnder)
import Data.Effect (Emb)
import Data.Effect.Except (Throw, throw)
import Data.Effect.State (State, get, modify, runStateIORef)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)

data KVStore k v :: Effect where
    LookupKV :: k -> KVStore k v f (Maybe v)
    UpdateKV :: k -> Maybe v -> KVStore k v f ()

makeEffectF ''KVStore

lookupOrThrowKV
    :: forall k v e es ff c
     . (KVStore k v :> es, Throw e :> es, Monad (Eff ff es), Free c ff)
    => (k -> e)
    -> k
    -> Eff ff es v
lookupOrThrowKV err k =
    lookupKV k >>= maybe (throw $ err k) pure
{-# INLINE lookupOrThrowKV #-}

existsKV :: forall v k es ff c. (KVStore k v :> es, Functor (Eff ff es), Free c ff) => k -> Eff ff es Bool
existsKV = fmap isJust . lookupKV @k @v
{-# INLINE existsKV #-}

writeKV :: forall k v es ff c. (KVStore k v :> es, Free c ff) => k -> v -> Eff ff es ()
writeKV k v = updateKV k (Just v)
{-# INLINE writeKV #-}

deleteKV :: forall v k es ff c. (KVStore k v :> es, Free c ff) => k -> Eff ff es ()
deleteKV k = updateKV @k @v k Nothing
{-# INLINE deleteKV #-}

modifyKV
    :: forall k v es ff c
     . (KVStore k v :> es, Monad (Eff ff es), Free c ff)
    => v
    -> (v -> v)
    -> k
    -> Eff ff es ()
modifyKV vDefault f k = do
    v <- lookupKV k
    updateKV k (Just $ maybe vDefault f v)
{-# INLINE modifyKV #-}

runKVStoreIORef
    :: forall k v a es ff c
     . (Ord k, Emb IO :> es, forall es'. Monad (Eff ff es'), Free c ff)
    => Map k v
    -> Eff ff (KVStore k v ': es) a
    -> Eff ff es (Map k v, a)
runKVStoreIORef initial =
    raiseUnder
        >>> runKVStoreAsState
        >>> runStateIORef initial
{-# INLINE runKVStoreIORef #-}

runKVStoreAsState
    :: forall k v es ff c
     . (Ord k, State (Map k v) :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (KVStore k v ': es) ~> Eff ff es
runKVStoreAsState = interpret \case
    LookupKV k -> get <&> Map.lookup k
    UpdateKV k v -> modify $ Map.update (const v) k
{-# INLINE runKVStoreAsState #-}
