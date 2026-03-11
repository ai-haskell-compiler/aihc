{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that can be used to hold environmental values in the context.
Environmental values are immutable and do not change across procedures, but you
can modify the value within a local scope using the `local` operation.
-}
module Data.Effect.Reader (
    module Data.Effect.Reader,
    Ask (..),
    Local (..),
)
where

import Control.Effect (sendFor)
import Control.Effect.Interpret (interposeFor)
import Data.Effect (Ask (Ask), Local (Local))
import Data.Effect.OpenUnion (IdentityResolver, Membership, membership)

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Ask
makeEffectH_' (def & noGenerateLabel & noGenerateOrderInstance) ''Local

-- | Obtains a value from the environment and returns it transformed by the given function.
asks
    :: forall r es ff a c
     . (Ask r :> es, Functor (Eff ff es), Free c ff)
    => (r -> a)
    -> Eff ff es a
asks f = f <$> ask
{-# INLINE asks #-}

-- | Interpret the t'Ask'/t'Local' effects.
runReader
    :: forall r es ff a c
     . (forall es'. Applicative (Eff ff es'), Free c ff)
    => r
    -> Eff ff (Local r ': Ask r ': es) a
    -> Eff ff es a
runReader r = runAsk r . runLocal
{-# INLINE runReader #-}

-- | Interpret the t'Ask' effect.
runAsk
    :: forall r es ff a c
     . (Applicative (Eff ff es), Free c ff)
    => r
    -> Eff ff (Ask r ': es) a
    -> Eff ff es a
runAsk r = interpret \Ask -> pure r
{-# INLINE runAsk #-}

-- | Interpret the t'Local' effect.
runLocal
    :: forall r es ff a c
     . (Applicative (Eff ff es), Ask r `In` es, Free c ff)
    => Eff ff (Local r ': es) a
    -> Eff ff es a
runLocal = interpret handleLocal
{-# INLINE runLocal #-}

-- | A handler for the t'Local' effect.
handleLocal
    :: forall r es ff c
     . (Applicative (Eff ff es), Ask r `In` es, Free c ff)
    => Local r ~~> Eff ff es
handleLocal = handleLocalFor $ membership @IdentityResolver
{-# INLINE handleLocal #-}

-- | A handler for the t'Local' effect.
handleLocalFor
    :: forall r es ff c
     . (Applicative (Eff ff es), Free c ff)
    => Membership (Ask r) es
    -> Local r ~~> Eff ff es
handleLocalFor pr (Local f a) = a & interposeFor pr \Ask -> f <$> sendFor pr Ask
{-# INLINE handleLocalFor #-}
