-- |
-- Bidirectional transforms for "Data.Monoid".
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe, TypeOperators, QuasiQuotes #-}
module Data.Invertible.Monoid
  ( BiEndo(..)
  , dual
  , endo
  , biEndo
  , all
  , any
  , sum
  , product
  , first
  , last
  , alt
  ) where

import Prelude hiding (fmap, (<$>), all, any, sum, product, last)
import qualified Control.Category as C
import Data.Monoid

import Data.Invertible.Bijection
import Data.Invertible.TH

-- |(Un)wrap the 'Dual' monoid.
dual :: a <-> Dual a
dual = [biCase|a <-> Dual a|]

-- |(Un)wrap the 'Endo' monoid.
endo :: (a -> a) <-> Endo a
endo = [biCase|a <-> Endo a|]

-- | The monoid of endomorphisms under composition.
newtype BiEndo a = BiEndo { appBiEndo :: a <-> a }

#if MIN_VERSION_base(4,11,0)
instance Semigroup (BiEndo a) where
  BiEndo f <> BiEndo g = BiEndo (f C.. g)
#endif

instance Monoid (BiEndo a) where
  mempty = BiEndo C.id
  BiEndo f `mappend` BiEndo g = BiEndo (f C.. g)

-- |(Un)wrap the 'BiEndo' monoid.
biEndo :: (a <-> a) <-> BiEndo a
biEndo = [biCase|a <-> BiEndo a|]

-- |(Un)wrap the 'All' monoid.
all :: Bool <-> All
all = [biCase|a <-> All a|]

-- |(Un)wrap the 'Any' monoid.
any :: Bool <-> Any
any = [biCase|a <-> Any a|]

-- |(Un)wrap the 'Sum' monoid.
sum :: a <-> Sum a
sum = [biCase|a <-> Sum a|]

-- |(Un)wrap the 'Product' monoid.
product :: a <-> Product a
product = [biCase|a <-> Product a|]

-- |(Un)wrap the 'First' monoid.
first :: Maybe a <-> First a
first = [biCase|a <-> First a|]

-- |(Un)wrap the 'Last' monoid.
last :: Maybe a <-> Last a
last = [biCase|a <-> Last a|]

-- |(Un)wrap the 'Last' monoid.
alt :: f a <-> Alt f a
alt = [biCase|a <-> Alt a|]

