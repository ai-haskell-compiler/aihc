{-# LANGUAGE PolyKinds #-}

module Semigroupoid where

import Control.Category
import Prelude hiding (id, (.))

class Semigroupoid c where
  o :: c j k -> c i j -> c i k

instance Semigroupoid (->) where
  o = (.)

newtype WrappedCategory k a b = WrapCategory {unwrapCategory :: k a b}

instance Category k => Semigroupoid (WrappedCategory k) where
  WrapCategory f `o` WrapCategory g = WrapCategory (f . g)

instance Category k => Category (WrappedCategory k) where
  id = WrapCategory id
  WrapCategory f . WrapCategory g = WrapCategory (f . g)

newtype Semi m a b = Semi {getSemi :: m}

instance Monoid m => Category (Semi m) where
  id = Semi mempty
  Semi m . Semi n = Semi (m `mappend` n)
