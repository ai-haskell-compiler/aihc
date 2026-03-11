{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Realizes [@unliftio@](https://hackage.haskell.org/package/unliftio) in the form of higher-order effects.
-}
module Data.Effect.Unlift (
    module Data.Effect.Unlift,
    UnliftBase (..),
    UnliftIO,
)
where

import Control.Effect (sendAt)
import Control.Effect.Interpret (runEff)
import Data.Effect (Emb (Emb), UnliftBase (WithRunInBase), UnliftIO)
import UnliftIO qualified as IO

makeEffectH_' (def & noGenerateLabel & noGenerateOrderInstance) ''UnliftBase

pattern WithRunInIO :: (f ~> IO -> IO a) -> UnliftIO f a
pattern WithRunInIO f = WithRunInBase f
{-# COMPLETE WithRunInIO #-}

withRunInIO
    :: forall es ff a c
     . (UnliftIO :> es, Free c ff)
    => (Eff ff es ~> IO -> IO a)
    -> Eff ff es a
withRunInIO = withRunInBase
{-# INLINE withRunInIO #-}

runUnliftBase :: forall b ff a c. (c b, Free c ff) => Eff ff '[UnliftBase b, Emb b] a -> b a
runUnliftBase =
    runEff . interpret \(WithRunInBase f) ->
        sendAt @0 $ Emb $ f runEff
{-# INLINE runUnliftBase #-}

runUnliftIO :: (IO.MonadUnliftIO m, Free c ff, c m) => Eff ff '[UnliftIO, Emb m] a -> m a
runUnliftIO =
    runEff . interpret \(WithRunInBase f) ->
        sendAt @0 $ Emb $ IO.withRunInIO \run -> f $ run . runEff
{-# INLINE runUnliftIO #-}
