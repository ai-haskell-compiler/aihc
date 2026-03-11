{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Example where

import Data.Effect (Effect)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.HFunctor.TH (makeHFunctor, makeHFunctor')
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Kind (Type)
import Data.List.Infinite (Infinite ((:<)))

data Throw e :: Effect where
    Throw :: e -> Throw e f a

data Catch e :: Effect where
    Catch :: f a -> (e -> f a) -> Catch e f a

makeEffectF ''Throw
makeEffectH ''Catch

data Unlift b :: Effect where
    WithRunInBase :: ((forall x. f x -> b x) -> b a) -> Unlift b f a

makeEffectH ''Unlift

data Nested :: Effect where
    Nested :: ([f a -> Int] -> Int) -> Nested f a
makeHFunctor ''Nested

data ManuallyCxt (g :: Type -> Type) h :: Effect where
    ManuallyCxt :: g (h f a) -> ManuallyCxt g h f a
makeHFunctor' ''ManuallyCxt \(g :< h :< _) -> [t|(Functor $g, HFunctor $h)|]

data NestedTuple :: Effect where
    NestedTuple :: ((forall x. (f x, f a) -> Int) -> Int) -> NestedTuple f a
makeHFunctor ''NestedTuple

newtype IdentityH f (a :: Type) = IdentityH {unIdentityH :: f a}
makeHFunctor ''IdentityH
