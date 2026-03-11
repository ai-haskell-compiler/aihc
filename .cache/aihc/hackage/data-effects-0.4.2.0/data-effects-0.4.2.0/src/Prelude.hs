{-# LANGUAGE PackageImports #-}

-- SPDX-License-Identifier: MPL-2.0

module Prelude (
    module Prelude,
    module Control.Effect,
    module Control.Effect.Interpret,
    module Data.Effect.OpenUnion,
    module Data.Effect,
    module Data.Effect.TH,
    module Data.Effect.HFunctor.TH,
    Type,
    Infinite ((:<)),
) where

import Control.Effect (Eff, Free, type (~>), type (~~>))
import Control.Effect.Interpret (interposeIn, interpret)
import Data.Effect (Effect, LabelOf)
import Data.Effect.HFunctor.TH
import Data.Effect.OpenUnion (Has, In, type (:>))
import Data.Effect.TH
import Data.Kind (Type)

import "base" Prelude
