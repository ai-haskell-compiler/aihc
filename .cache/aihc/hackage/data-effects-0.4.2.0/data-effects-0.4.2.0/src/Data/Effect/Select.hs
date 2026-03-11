{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.Select where

data Select r :: Effect where
    Select :: ((a -> r) -> a) -> Select r f a

makeEffectF ''Select
