module Numeric.Netlib.Class where

import Foreign.Storable.Complex ()
import Foreign.Storable (Storable)

import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Complex (Complex)

import qualified Prelude
import Prelude (Float, Double, ($))


class (Floating a, Prelude.RealFloat a) => Real a where
   switchReal :: f Float -> f Double -> f a

instance Real Float where switchReal f _ = f
instance Real Double where switchReal _ f = f


class (Storable a, Prelude.Fractional a) => Floating a where
   switchFloating ::
      f Float -> f Double ->
      f (Complex Float) -> f (Complex Double) -> f a

instance Floating Float where switchFloating f _ _ _ = f
instance Floating Double where switchFloating _ f _ _ = f
instance (Real a) => Floating (Complex a) where
   switchFloating _ _ fz fc = getCompose $ switchReal (Compose fz) (Compose fc)
