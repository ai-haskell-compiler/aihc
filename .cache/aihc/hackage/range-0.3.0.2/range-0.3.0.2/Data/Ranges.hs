{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- | This module provides a simpler interface than the 'Data.Range' module, allowing you to work with
-- multiple ranges at the same time.
--
-- One of the main advantages of this module is that it implements 'Monoid' for 'Ranges' which lets you
-- write code like:
-- 
module Data.Ranges (
  -- * Range creation
  (+=+),
  (+=*),
  (*=+),
  (*=*),
  lbi,
  lbe,
  ubi,
  ube,
  inf,
  -- * Comparison functions
  inRanges,
  aboveRanges,
  belowRanges,
  -- * Set operations
  union,
  intersection,
  difference,
  invert,
  -- * Enumerable methods
  fromRanges,
  joinRanges,
  -- * Data types
  Ranges(..)
) where

import Data.Semigroup

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import qualified Data.Range as R

-- TODO Can we make this use a Range Algebra internally ?
newtype Ranges a = Ranges { unRanges :: [R.Range a] }

instance Show a => Show (Ranges a) where
   showsPrec i (Ranges xs) = ((++) "Ranges ") . showsPrec i xs

instance Ord a => Semigroup (Ranges a) where
   (<>) (Ranges a) (Ranges b) = Ranges . R.mergeRanges $ a ++ b

instance Ord a => Monoid (Ranges a) where
   mempty = Ranges []
   mappend (Ranges a) (Ranges b) = Ranges . R.mergeRanges $ a ++ b
   mconcat = Ranges . R.mergeRanges . concat . fmap unRanges

instance Functor Ranges where
   fmap f (Ranges xs) = Ranges . fmap (fmap f) $ xs

(+=+) :: a -> a -> Ranges a
(+=+) a b = Ranges . pure $ (R.+=+) a b

(+=*) :: a -> a -> Ranges a
(+=*) a b = Ranges . pure $ (R.+=*) a b

(*=+) :: a -> a -> Ranges a
(*=+) a b = Ranges . pure $ (R.*=+) a b

(*=*) :: a -> a -> Ranges a
(*=*) a b = Ranges . pure $ (R.*=*) a b

lbi :: a -> Ranges a
lbi = Ranges . pure . R.lbi

lbe :: a -> Ranges a
lbe = Ranges . pure . R.lbe

ubi :: a -> Ranges a
ubi = Ranges . pure . R.ubi

ube :: a -> Ranges a
ube = Ranges . pure . R.ube

inf :: Ranges a
inf = Ranges [R.inf]

inRanges :: (Ord a) => Ranges a -> a -> Bool
inRanges (Ranges xs) = R.inRanges xs

-- | Checks if the value provided is above all of the ranges provided.
aboveRanges :: (Ord a) => Ranges a -> a -> Bool
aboveRanges (Ranges xs) a = R.aboveRanges xs a

-- | Checks if the value provided is below all of the ranges provided.
belowRanges :: (Ord a) => Ranges a -> a -> Bool
belowRanges (Ranges rs) a = R.belowRanges rs a

union :: (Ord a) => Ranges a -> Ranges a -> Ranges a
union (Ranges a) (Ranges b) = Ranges $ R.union a b

intersection :: (Ord a) => Ranges a -> Ranges a -> Ranges a
intersection (Ranges a) (Ranges b) = Ranges $ R.intersection a b

difference :: (Ord a) => Ranges a -> Ranges a -> Ranges a
difference (Ranges a) (Ranges b) = Ranges $ R.difference a b

invert :: (Ord a) => Ranges a -> Ranges a
invert = Ranges . R.invert . unRanges

fromRanges :: (Ord a, Enum a) => Ranges a -> [a]
fromRanges = R.fromRanges . unRanges

joinRanges :: (Ord a, Enum a) => Ranges a -> Ranges a
joinRanges = Ranges . R.joinRanges . unRanges