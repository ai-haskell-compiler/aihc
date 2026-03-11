-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BoundingBox
-- Copyright   :  (C) 2014 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
-- The type and accessors for bounding boxes
----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, Rank2Types #-}
module Data.BoundingBox where

import Control.Lens
import Data.Foldable
import Data.Typeable
import Control.Applicative
import Data.Foldable as Foldable
import Data.Traversable as Traversable

-- | The type of bounding box for arbitrary vector @f@.
-- The functions for this type assume that @f@ is a "zipping" 'Applicative'.
data Box f a = Box (f a) (f a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Read)

instance Applicative f => Applicative (Box f) where
    pure a = Box (pure a) (pure a)
    {-# INLINE pure #-}
    Box f g <*> Box a b = Box (f <*> a) (g <*> b)
    {-# INLINE (<*>) #-}

instance Monad f => Monad (Box f) where
    return a = Box (return a) (return a)
    {-# INLINE return #-}
    Box f g >>= k = Box (f >>= \x -> let Box p _ = k x in p) (g >>= \x -> let Box _ q = k x in q)

-- | check whether the point is in the box.
isInside :: (Applicative f, Foldable f, Ord a) => f a -> Box f a -> Bool
isInside v (Box p q) = Foldable.and (liftA2 (<=) p v) && Foldable.and (liftA2 (<=) v q)

-- | Extend each side.
inflate :: (Functor f, Num a) => a -> Box f a -> Box f a
inflate t (Box p q) = Box (fmap (subtract t) p) (fmap (+t) q)

-- | Returns True if the bounding box is valid.
isCanonical :: (Applicative f, Foldable f, Ord a) => Box f a -> Bool
isCanonical (Box p q) = Foldable.and (liftA2 (<=) p q)

-- | Calculate an union between two boundingboxes.
union :: (Applicative f, Ord a) => Box f a -> Box f a -> Box f a
union (Box p q) (Box r s) = Box (liftA2 min p r) (liftA2 max q s)

-- | Calculate an intersect between two boundingboxes.
intersect :: (Applicative f, Ord a) => Box f a -> Box f a -> Box f a
intersect (Box p q) (Box r s) = Box (liftA2 max p r) (liftA2 min q s)

-- | Enumerate the corners.
corners :: (Applicative f, Traversable f) => Box f a -> [f a]
corners (Box p q) = Traversable.sequence $ liftA2 (\a b -> [a, b]) p q

sizePos :: (Applicative f, Num a) => f a -> Iso' (Box f a) (f a, f a)
sizePos k = iso f g where
    f (Box p q) = (liftA2 (-) q p, (+) <$> liftA2 (*) (fmap (1-) k) p <*> liftA2 (*) k q)
    g (s, v) = Box ((-) <$> v <*> liftA2 (*) k s) ((+) <$> v <*> liftA2 (*) (fmap (1-) k) s)

-- | The accessor for the position on the given reference. Usually the reference point
position :: (Applicative f, Num a) => f a -> Lens' (Box f a) (f a)
position ref = sizePos ref . _2

-- | The accessor for the size. A given reference point will be a center of resizing.
size :: (Applicative f, Num a) => f a -> Lens' (Box f a) (f a)
size ref = sizePos ref . _1
