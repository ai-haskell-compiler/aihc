{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [@co-log@](https://hackage.haskell.org/package/co-log) ecosystem.

The interface is similar to [@co-log-polysemy@](https://hackage.haskell.org/package/co-log-polysemy).
-}
module Data.Effect.Log where

import Colog.Core (LogAction (LogAction))
import Control.Effect (emb)
import Data.Effect (Emb)
import Data.Effect.Output (Output (Output), output)
import Prelude hiding (log)

data Log msg :: Effect where
    Log :: msg -> Log msg f ()
makeEffectF ''Log

runLogAsOutput :: forall msg a es ff c. (Output msg :> es, Free c ff) => Eff ff (Log msg ': es) a -> Eff ff es a
runLogAsOutput = interpret \(Log msg) -> output msg
{-# INLINE runLogAsOutput #-}

runOutputAsLog :: forall msg a es ff c. (Log msg :> es, Free c ff) => Eff ff (Output msg ': es) a -> Eff ff es a
runOutputAsLog = interpret \(Output msg) -> log msg
{-# INLINE runOutputAsLog #-}

runLogAction :: forall msg a es ff c. (Free c ff) => LogAction (Eff ff es) msg -> Eff ff (Log msg ': es) a -> Eff ff es a
runLogAction (LogAction f) = interpret \(Log msg) -> f msg
{-# INLINE runLogAction #-}

runLogActionEmbed :: forall msg f a es ff c. (Emb f :> es, Free c ff) => LogAction f msg -> Eff ff (Log msg ': es) a -> Eff ff es a
runLogActionEmbed (LogAction f) = interpret \(Log msg) -> emb $ f msg
{-# INLINE runLogActionEmbed #-}
