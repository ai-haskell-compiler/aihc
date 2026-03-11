module Lens.Family.Identical where

import Control.Applicative.Backwards (Backwards(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))

-- It would really be much better if comonads was in tranformers
class (Traversable f, Applicative f) => Identical f where
  extract :: f a -> a

instance Identical Identity where
  extract (Identity x) = x

instance Identical f => Identical (Backwards f) where
  extract (Backwards x) = extract x

instance (Identical f, Identical g) => Identical (Compose f g) where
  extract (Compose x) = extract (extract x)
