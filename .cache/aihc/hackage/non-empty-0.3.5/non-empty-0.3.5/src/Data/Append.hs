module Data.Append where

import qualified Data.NonEmpty.Class as C
import Control.Applicative (liftA2)
import Control.DeepSeq (NFData, rnf, )
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mappend)

import Prelude hiding (fst, snd)


data T f g a =
   Cons {
      fst :: f a,
      snd :: g a
   }

instance (Functor f, Functor g) => Functor (T f g) where
   fmap f (Cons xs ys) = Cons (fmap f xs) (fmap f ys)

instance (Foldable f, Foldable g) => Foldable (T f g) where
   foldMap f (Cons xs ys) = mappend (foldMap f xs) (foldMap f ys)

instance (Traversable f, Traversable g) => Traversable (T f g) where
   traverse f (Cons xs ys) = liftA2 Cons (traverse f xs) (traverse f ys)


instance (C.NFData f, C.NFData g) => C.NFData (T f g) where
   rnf (Cons f g) = rnf (C.rnf f, C.rnf g)

instance (C.NFData f, C.NFData g, NFData a) => NFData (T f g a) where
   rnf = C.rnf
