{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp

This module provides the t`Input` effect, comes
from [@Polysemy.Input@](https://hackage.haskell.org/package/polysemy-1.9.1.1/docs/Polysemy-Input.html)
in the @polysemy@ package.

Realizes input of values from the external world.
-}
module Data.Effect.Input where

-- | A general effect representing input of values from the external world.
data Input i :: Effect where
    -- | Retrieve a value input from the external world.
    Input :: Input i f i

makeEffectF ''Input

-- | Returns the value obtained by transforming the input value using the given function.
inputs
    :: forall i es ff a c
     . (Input i :> es, Functor (Eff ff es), Free c ff)
    => (i -> a)
    -> Eff ff es a
inputs f = f <$> input
{-# INLINE inputs #-}

-- | Interprets the t'Input' effect by executing the given input handler each time an input is required.
runInputEff
    :: forall i es ff a c
     . (Free c ff)
    => Eff ff es i
    -> Eff ff (Input i ': es) a
    -> Eff ff es a
runInputEff a = interpret \Input -> a
{-# INLINE runInputEff #-}

-- | Interprets the t'Input' effect by providing the given constant as input.
runInputConst
    :: forall i es ff a c
     . (Applicative (Eff ff es), Free c ff)
    => i
    -> Eff ff (Input i ': es) a
    -> Eff ff es a
runInputConst i = interpret \Input -> pure i
{-# INLINE runInputConst #-}
