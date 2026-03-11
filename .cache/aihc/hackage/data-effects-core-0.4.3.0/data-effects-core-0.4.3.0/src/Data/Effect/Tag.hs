-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2024 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.Tag where

import Data.Effect (Effect, FirstOrder, LabelOf, OrderOf)
import Data.Effect.HFunctor (HFunctor)
import Data.Kind (Type)

-- | Tagged effect.
newtype Tagged tag (e :: Effect) f a = Tag {unTag :: e f a}
    deriving stock (Functor, Foldable, Traversable)
    deriving newtype (HFunctor)

type instance OrderOf (Tagged tag e) = OrderOf e
instance (FirstOrder e) => FirstOrder (Tagged tag e)

type instance LabelOf (Tagged tag e) = TaggedLabel tag (LabelOf e)
data TaggedLabel tag (label :: Type)

-- | Tagged effect.
type e # tag = Tagged tag e

infixl 7 #
