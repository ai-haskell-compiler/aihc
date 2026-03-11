{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Interpret where

import Control.Effect (
    Eff (..),
    Free (liftFree),
    hoist,
    runFree,
    type (~>),
    type (~~>),
 )
import Data.Effect (Emb, getEmb)
import Data.Effect.OpenUnion (
    Has,
    In,
    KnownLength,
    KnownOrder,
    Membership,
    Suffix,
    Union,
    hfmapUnion,
    identityMembership,
    keyMembership,
    labelMembership,
    nil,
    project,
    weakens,
    (!++),
    (!:),
    (:>),
    type (++),
 )
import Data.Effect.Tag (unTag)
import Data.Functor.Identity (Identity, runIdentity)

runEff :: (Free c ff, c f) => Eff ff '[Emb f] a -> f a
runEff = runFree (getEmb !: nil) . unEff
{-# INLINE runEff #-}

runPure :: (Free c ff, c Identity) => Eff ff '[] a -> a
runPure = runIdentity . runFree nil . unEff
{-# INLINE runPure #-}

interpret
    :: forall e es ff a c
     . (KnownOrder e, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff (e ': es) a
    -> Eff ff es a
interpret i = interpretAll $ i !: Eff . liftFree
{-# INLINE interpret #-}

reinterpret
    :: forall e es es' ff a c
     . (Suffix es es', KnownOrder e, Free c ff)
    => e ~~> Eff ff es'
    -> Eff ff (e ': es) a
    -> Eff ff es' a
reinterpret i = interpretAll $ i !: Eff . liftFree . weakens
{-# INLINE reinterpret #-}

interprets
    :: forall es r ff a c
     . (KnownLength es, Free c ff)
    => Union es ~~> Eff ff r
    -> Eff ff (es ++ r) a
    -> Eff ff r a
interprets i = interpretAll $ i !++ Eff . liftFree
{-# INLINE interprets #-}

reinterprets
    :: forall es r r' ff a c
     . (Suffix r r', KnownLength es, Free c ff)
    => (Union es (Eff ff r') ~> Eff ff r')
    -> Eff ff (es ++ r) a
    -> Eff ff r' a
reinterprets i = interpretAll $ i !++ Eff . liftFree . weakens @r
{-# INLINE reinterprets #-}

interpose
    :: forall e es ff a c
     . (e :> es, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
interpose = interposeFor labelMembership
{-# INLINE interpose #-}

interposeOn
    :: forall key e es ff a c
     . (Has key e es, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
interposeOn f = interposeFor (keyMembership @key) (f . unTag)
{-# INLINE interposeOn #-}

interposeIn
    :: forall e es ff a c
     . (e `In` es, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
interposeIn = interposeFor identityMembership
{-# INLINE interposeIn #-}

interposeFor
    :: forall e es ff a c
     . (KnownOrder e, Free c ff)
    => Membership e es
    -> e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
interposeFor i f =
    interpretAll \u ->
        case project i u of
            Just e -> f e
            Nothing -> Eff $ liftFree u
{-# INLINE interposeFor #-}

preinterpose
    :: forall e es ff a c
     . (e :> es, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
preinterpose = preinterposeFor labelMembership
{-# INLINE preinterpose #-}

preinterposeOn
    :: forall key e es ff a c
     . (Has key e es, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
preinterposeOn f = preinterposeFor (keyMembership @key) (f . unTag)
{-# INLINE preinterposeOn #-}

preinterposeIn
    :: forall e es ff a c
     . (e `In` es, Free c ff)
    => e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
preinterposeIn = preinterposeFor identityMembership
{-# INLINE preinterposeIn #-}

preinterposeFor
    :: forall e es ff a c
     . (KnownOrder e, Free c ff)
    => Membership e es
    -> e ~~> Eff ff es
    -> Eff ff es a
    -> Eff ff es a
preinterposeFor i f = go
  where
    go :: Eff ff es ~> Eff ff es
    go (Eff a) = Eff $ (`runFree` a) \u ->
        hoist (hfmapUnion go) $ case project i u of
            Just e -> unEff $ f e
            Nothing -> liftFree u
{-# INLINE preinterposeFor #-}

interpretAll
    :: forall es es' ff a c
     . (Free c ff)
    => (Union es ~~> Eff ff es')
    -> Eff ff es a
    -> Eff ff es' a
interpretAll i = go
  where
    go :: Eff ff es ~> Eff ff es'
    go = Eff . runFree (unEff . i . hfmapUnion go) . unEff
{-# INLINE interpretAll #-}

iterAllEff
    :: forall es f ff a c
     . (Free c ff, c f)
    => Union es ~~> f
    -> Eff ff es a
    -> f a
iterAllEff i = go
  where
    go :: Eff ff es ~> f
    go = runFree (i . hfmapUnion go) . unEff
{-# INLINE iterAllEff #-}
