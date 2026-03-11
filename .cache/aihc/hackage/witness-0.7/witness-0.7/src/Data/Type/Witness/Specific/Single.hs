{-# OPTIONS -fno-warn-orphans #-}

module Data.Type.Witness.Specific.Single where

import Data.Type.Witness.Specific.All
import Import

type SingleType :: forall k. k -> k -> Type
type SingleType = (:~:)

singleAllOf :: t -> AllOf (SingleType t)
singleAllOf t = MkAllOf $ \Refl -> t

getSingleAllOf :: AllOf (SingleType t) -> t
getSingleAllOf (MkAllOf f) = f Refl
