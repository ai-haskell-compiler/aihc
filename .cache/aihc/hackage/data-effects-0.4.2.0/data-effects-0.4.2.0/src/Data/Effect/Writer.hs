{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

Effects that can accumulate values monoidally in a context.
-}
module Data.Effect.Writer (
    module Data.Effect.Writer,
    Tell (..),
    WriterH (..),
    pass,
)
where

import Control.Effect (pass)
import Data.Effect (Tell (Tell), WriterH (Censor, Listen))

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''Tell
makeEffectH_' (def & noGenerateLabel & noGenerateOrderInstance) ''WriterH

-- | 'censor' with pre-applying semantics.
censorPre
    :: forall w es ff a c
     . (Tell w `In` es, Free c ff)
    => (w -> w)
    -> Eff ff es a
    -> Eff ff es a
censorPre f = interposeIn @(Tell w) \(Tell w) -> tell'_ $ f w
{-# INLINE censorPre #-}
