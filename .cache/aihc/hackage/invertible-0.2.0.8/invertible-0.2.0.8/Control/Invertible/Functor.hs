-- |
-- This provides a subset of the functionality as the invariant package's "Data.Functor.Invariant" module, but based on "Data.Invertible", without all the instances, and with an interface matching "Data.Functor".
--
-- This module is intended to be imported qualified, e.g.,:
-- 
-- > import qualified Control.Invertible.Functor as Inv
--
{-# LANGUAGE CPP, TypeOperators, FlexibleInstances #-}
#if !(defined(VERSION_semigroupoids) && MIN_VERSION_semigroupoids(5,2,2))
{-# LANGUAGE Safe #-}
#endif
module Control.Invertible.Functor
  ( Functor(..)
  , fmapDefault
  , (<$>)
  ) where

import qualified Prelude
import Prelude hiding ((.), Functor(..), (<$>))
import Control.Arrow (Arrow)
import Control.Category ((.))
import Data.Monoid (Endo(..))

#ifdef VERSION_semigroupoids
import Data.Semigroupoid (Semigroupoid)
#endif
import Control.Invertible.BiArrow ((^^<<), invert)
import Data.Invertible.Bijection
import Data.Invertible.Monoid (BiEndo(..))

-- |An invariant version of 'Data.Functor.Functor', equivalent to 'Data.Functor.Inviarant.Invariant'.
class Functor f where
  fmap :: a <-> b -> f a -> f b

-- |Default invertible 'Functor' implementation for simple non-invertible 'Prelude.Functor's.
fmapDefault :: Prelude.Functor f => a <-> b -> f a -> f b
fmapDefault (f :<->: _) x = f Prelude.<$> x

-- |An infix synnonym for 'fmap'.
(<$>) :: Functor f => a <-> b -> f a -> f b
(<$>) = fmap
infixl 4 <$>

instance (
#ifdef VERSION_semigroupoids
    Semigroupoid a,
#endif
    Arrow a) => Functor (Bijection a b) where
  fmap = (^^<<)

instance Functor Endo where
  fmap (f :<->: g) (Endo a) = Endo $ f . a . g

instance Functor BiEndo where
  fmap f (BiEndo a) = BiEndo $ f . a . invert f
