-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects for holding mutable state values in the context.
-}
module Data.Effect.State (
    module Data.Effect.State,
    State (..),
) where

import Data.Effect (Ask (Ask), Emb, Local (Local), State (Get, Put))
import Data.Functor ((<&>))
import UnliftIO (newIORef, readIORef, writeIORef)

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''State

-- | Retrieves the current state value from the context and returns the value transformed based on the given function.
gets :: forall s es a ff c. (State s :> es, Functor (Eff ff es), Free c ff) => (s -> a) -> Eff ff es a
gets f = f <$> get
{-# INLINE gets #-}

-- | Modifies the current state value in the context based on the given function.
modify :: forall s es ff c. (State s :> es, Monad (Eff ff es), Free c ff) => (s -> s) -> Eff ff es ()
modify f = put . f =<< get
{-# INLINE modify #-}

-- | Interpret the 'State' effect based on an IO-fused semantics using t'Data.IORef.IORef'.
runStateIORef
    :: forall s es ff a c
     . (Emb IO :> es, Monad (Eff ff es), Free c ff)
    => s
    -> Eff ff (State s ': es) a
    -> Eff ff es (s, a)
runStateIORef s0 m = do
    ref <- newIORef s0
    a <-
        m & interpret \case
            Get -> readIORef ref
            Put s -> writeIORef ref s
    readIORef ref <&> (,a)
{-# INLINE runStateIORef #-}

{- |
Interpret the 'State' effect based on an IO-fused semantics using t'Data.IORef.IORef'.
Do not include the final state in the return value.
-}
evalStateIORef
    :: forall s es ff a c
     . (Emb IO :> es, Monad (Eff ff es), Free c ff)
    => s
    -> Eff ff (State s ': es) a
    -> Eff ff es a
evalStateIORef s0 m = do
    ref <- newIORef s0
    m & interpret \case
        Get -> readIORef ref
        Put s -> writeIORef ref s
{-# INLINE evalStateIORef #-}

execStateIORef
    :: forall s es ff a c
     . (Emb IO :> es, Monad (Eff ff es), Free c ff)
    => s
    -> Eff ff (State s ': es) a
    -> Eff ff es s
execStateIORef s0 = fmap fst . runStateIORef s0
{-# INLINE execStateIORef #-}

localToState
    :: forall r es ff a c
     . (State r `In` es, Monad (Eff ff es), Free c ff)
    => Eff ff (Local r ': es) a
    -> Eff ff es a
localToState =
    interpret \(Local f a) -> do
        save <- get'_ @r
        put'_ $ f save
        a <* put'_ save
{-# INLINE localToState #-}

askToGet
    :: forall r es ff a c
     . (State r `In` es, Free c ff)
    => Eff ff (Ask r ': es) a
    -> Eff ff es a
askToGet = interpret \Ask -> get'_
{-# INLINE askToGet #-}
