{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the t`Output` effect, comes
from [@Polysemy.Output@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Output.html)
in the @polysemy@ package.

Realizes output of values to the external world.
-}
module Data.Effect.Output where

-- | A general effect representing output of values to the external world.
data Output o :: Effect where
    -- | Output a value to the external world.
    Output :: o -> Output o f ()

makeEffectF ''Output

-- | Interprets the t'Output' effect using the given output handler.
runOutputEff
    :: forall o es ff a c
     . (Free c ff)
    => (o -> Eff ff es ())
    -> Eff ff (Output o ': es) a
    -> Eff ff es a
runOutputEff f = interpret \(Output o) -> f o
{-# INLINE runOutputEff #-}

-- | Interprets the t'Output' effect by ignoring the outputs.
ignoreOutput
    :: forall o es ff a c
     . (Applicative (Eff ff es), Free c ff)
    => Eff ff (Output o ': es) a
    -> Eff ff es a
ignoreOutput = runOutputEff $ const $ pure ()
{-# INLINE ignoreOutput #-}
