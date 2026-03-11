-- | Some higher-kinded Functor types that make do until we get FunctorOf
--
-- eg https://eevie.ro/posts/2019-05-12-functor-of.html
module Box.Functor
  ( FFunctor (..),
    FoldableM (..),
  )
where

import Data.Kind
import Prelude

-- | An endofunctor in the category of endofunctors.
--
--  Like `Control.Monad.Morph.MFunctor` but without a `Monad` constraint.
class FFunctor (h :: (Type -> Type) -> Type -> Type) where
  foist :: (forall x. f x -> g x) -> h f a -> h g a

-- | Monadically Foldable
class FoldableM (t :: (Type -> Type) -> Type -> Type) where
  foldrM :: (Monad m) => (a -> m b -> m b) -> m b -> t m a -> m b
