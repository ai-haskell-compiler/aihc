module Bowtie.Foldable
  ( cataM
  , Base1
  , Recursive1 (..)
  , Corecursive1 (..)
  , cata1
  , cata1M
  , fmapViaBi
  , foldrViaBi
  , traverseViaBi
  )
where

import Control.Monad ((>=>))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Foldable (Base, Recursive (..))
import Data.Kind (Type)

cataM :: (Monad m, Recursive t, Base t ~ f, Traversable f) => (f k -> m k) -> t -> m k
cataM f = cata (sequence >=> f)

-- | 'Base' for Bifunctors
type family Base1 (f :: Type -> Type) :: Type -> Type -> Type

-- | 'Recursive' for Bifunctors
class (Bifunctor (Base1 f), Functor f) => Recursive1 f where
  project1 :: f a -> Base1 f a (f a)

-- | 'Corecursive' for Bifunctors
class (Bifunctor (Base1 f), Functor f) => Corecursive1 f where
  embed1 :: Base1 f a (f a) -> f a

-- | 'cata' for Bifunctors
cata1 :: (Recursive1 f, Base1 f ~ g) => (g a b -> b) -> f a -> b
cata1 f = go where go = f . second go . project1

-- | 'cataM' for Bifunctors
cata1M :: (Monad m, Recursive1 f, Base1 f ~ g, Bitraversable g) => (g a b -> m b) -> f a -> m b
cata1M f = go where go = bitraverse pure go . project1 >=> f

-- | A useful default 'fmap'
fmapViaBi :: (Recursive1 f, Corecursive1 f, Base1 f ~ g) => (a -> b) -> f a -> f b
fmapViaBi f = go where go = embed1 . bimap f go . project1

-- | A useful default 'foldr'
foldrViaBi :: (Recursive1 f, Base1 f ~ g, Bifoldable g) => (a -> b -> b) -> b -> f a -> b
foldrViaBi f = flip go where go fa b = bifoldr f go b (project1 fa)

-- | A useful default 'traverse'
traverseViaBi
  :: (Recursive1 f, Corecursive1 f, Base1 f ~ g, Bitraversable g, Applicative m) => (a -> m b) -> f a -> m (f b)
traverseViaBi f = go where go = fmap embed1 . bitraverse f go . project1
