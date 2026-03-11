{-# LANGUAGE UndecidableInstances #-}

module Bowtie.Knot
  ( Knot (..)
  , mkKnot
  , unMkKnot
  , transKnot
  )
where

import Bowtie.Foldable (Base1, Corecursive1 (..), Recursive1 (..), cata1, fmapViaBi, foldrViaBi, traverseViaBi)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Prettyprinter (Pretty (..))

-- | A fixpoint for a Bifunctor where the second type variable contains
-- the recursive structure.
type Knot :: (Type -> Type -> Type) -> Type -> Type
newtype Knot g a = Knot {unKnot :: g a (Knot g a)}

deriving newtype instance (Eq (g a (Knot g a))) => Eq (Knot g a)

deriving newtype instance (Ord (g a (Knot g a))) => Ord (Knot g a)

deriving stock instance (Show (g a (Knot g a))) => Show (Knot g a)

deriving newtype instance (Pretty (g a (Knot g a))) => Pretty (Knot g a)

deriving newtype instance (IsString (g a (Knot g a))) => IsString (Knot g a)

type instance Base1 (Knot g) = g

instance (Bifunctor g) => Recursive1 (Knot g) where project1 = unKnot

instance (Bifunctor g) => Corecursive1 (Knot g) where embed1 = Knot

instance (Bifunctor g) => Functor (Knot g) where fmap = fmapViaBi

instance (Bifunctor g, Bifoldable g) => Foldable (Knot g) where foldr = foldrViaBi

instance (Bitraversable g) => Traversable (Knot g) where traverse = traverseViaBi

-- | Pull a recursive structure apart and retie as a 'Knot'.
mkKnot :: (Recursive1 f, Base1 f ~ g) => f a -> Knot g a
mkKnot = cata1 Knot

-- | Go the other way.
unMkKnot :: (Corecursive1 f, Base1 f ~ g) => Knot g a -> f a
unMkKnot = cata1 embed1

-- | Transform the base Bifunctor.
transKnot :: (Bifunctor g) => (forall x y. g x y -> h x y) -> Knot g a -> Knot h a
transKnot nat = go
 where
  go = Knot . nat . second go . unKnot
