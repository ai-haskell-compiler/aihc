{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Fail (
    module Data.Effect.Fail,
    Fail (..),
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Effect (Emb, Fail (Fail))
import Prelude hiding (fail)
import Prelude qualified as IO

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Fail

runFailIO
    :: forall es a ff c
     . (Emb IO :> es, Monad (Eff ff es), Free c ff)
    => Eff ff (Fail ': es) a
    -> Eff ff es a
runFailIO = interpret \(Fail s) -> liftIO $ IO.fail s
{-# INLINE runFailIO #-}
