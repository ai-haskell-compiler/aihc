{-# LANGUAGE UndecidableInstances #-}

module Bowtie.Fix
  ( Fix (..)
  , mkFix
  , unMkFix
  , transFix
  )
where

import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Prettyprinter (Pretty (..))

-- | A basic Functor fixpoint like you'd see anywhere.
type Fix :: (Type -> Type) -> Type
newtype Fix f = Fix {unFix :: f (Fix f)}

deriving newtype instance (Eq (f (Fix f))) => Eq (Fix f)

deriving newtype instance (Ord (f (Fix f))) => Ord (Fix f)

deriving stock instance (Show (f (Fix f))) => Show (Fix f)

deriving newtype instance (Pretty (f (Fix f))) => Pretty (Fix f)

deriving newtype instance (IsString (f (Fix f))) => IsString (Fix f)

type instance Base (Fix f) = f

instance (Functor f) => Recursive (Fix f) where project = unFix

instance (Functor f) => Corecursive (Fix f) where embed = Fix

-- | Pull a recursive structure apart and retie as a 'Fix'.
mkFix :: (Recursive t, Base t ~ f) => t -> Fix f
mkFix = cata Fix

-- | Go the other way.
unMkFix :: (Corecursive t, Base t ~ f) => Fix f -> t
unMkFix = cata embed

-- | Transform the base Functor.
transFix :: (Functor f) => (forall x. f x -> g x) -> Fix f -> Fix g
transFix nat = go
 where
  go = Fix . nat . fmap go . unFix
