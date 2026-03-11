module Lens.Family.Phantom where

import Control.Applicative.Backwards (Backwards(..))
import Control.Applicative (Const(..))
import Data.Functor.Constant (Constant(..))
import Data.Functor.Compose (Compose(..))

class Functor f => Phantom f where
  coerce :: f a -> f b

instance Phantom f => Phantom (Backwards f) where
  coerce (Backwards x) = Backwards (coerce x)

instance Phantom (Const a) where
  coerce (Const x) = (Const x)

instance Phantom (Constant a) where
  coerce (Constant x) = (Constant x)

instance (Phantom f, Functor g) => Phantom (Compose f g) where
  coerce (Compose x) = Compose (coerce x)
