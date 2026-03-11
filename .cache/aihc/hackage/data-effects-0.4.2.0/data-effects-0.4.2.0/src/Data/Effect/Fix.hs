{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Fix (
    module Data.Effect.Fix,
    Fix (..),
)
where

import Data.Effect (Fix (Efix))

makeEffectH_' (def & noGenerateLabel & noGenerateOrderInstance) ''Fix
