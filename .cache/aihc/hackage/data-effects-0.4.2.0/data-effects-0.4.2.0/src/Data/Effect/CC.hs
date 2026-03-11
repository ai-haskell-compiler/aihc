{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.CC (
    module Data.Effect.CC,
    CC (SubFork, Jump),
    callCC_,
    sub,
)
where

import Control.Effect (callCC_, sub)
import Data.Effect (CC (Jump, SubFork))
import Data.Function (fix)

makeEffectF_' (def & noGenerateLabel & noGenerateOrderInstance) ''CC

callCC
    :: forall a ref es ff c
     . (CC ref :> es, Monad (Eff ff es), Free c ff)
    => ((forall b. a -> Eff ff es b) -> Eff ff es a)
    -> Eff ff es a
callCC f = sub (\x -> f $ jump x) pure
{-# INLINE callCC #-}

getCC
    :: forall a ref es ff c
     . (CC ref :> es, Monad (Eff ff es), Free c ff)
    => Eff ff es (Eff ff es a)
getCC = callCC_ $ pure . fix
{-# INLINE getCC #-}
