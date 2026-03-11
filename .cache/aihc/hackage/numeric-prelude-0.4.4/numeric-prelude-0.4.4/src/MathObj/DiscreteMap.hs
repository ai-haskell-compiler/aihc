{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- FIXME:
Rationale for -fno-warn-orphans:
 * The orphan instances can't be put into Numeric.NonNegative.Wrapper
   since that's in another package.
 * We had to spread the instance declarations
   over the modules defining the typeclasses instantiated.
   Do we want that?
 * We could define the DiscreteMap as newtype.
-}

{- |
DiscreteMap was originally intended as a type class
that unifies Map and Array.
One should be able to simply choose between
 - Map for sparse arrays
 - Array for full arrays.

However, the Edison package provides the class AssocX
which already exists for that purpose.

Currently I use this module for some numeric instances of Data.Map.
-}
module MathObj.DiscreteMap where

import qualified Algebra.NormedSpace.Sum       as NormedSum
import qualified Algebra.NormedSpace.Euclidean as NormedEuc
import qualified Algebra.NormedSpace.Maximum   as NormedMax
import qualified Algebra.VectorSpace           as VectorSpace
import qualified Algebra.Module                as Module
import qualified Algebra.Vector                as Vector
import qualified Algebra.Algebraic             as Algebraic
import qualified Algebra.Additive              as Additive

import Algebra.Module   ((*>))
import Algebra.Additive (zero,(+),negate)
import qualified Data.Map as Map
import Data.Map (Map)

import NumericPrelude.Base

-- FIXME: Should this be implemented by isZero?
-- | Remove all zero values from the map.
strip :: (Ord i, Eq v, Additive.C v) => Map i v -> Map i v
strip = Map.filter (zero /=)
--strip = Map.filter (((0 /=) .) . (flip const))

instance (Ord i, Eq v, Additive.C v) => Additive.C (Map i v) where
   zero = Map.empty
   (+)  = (strip.). Map.unionWith (+)
   --(+) y x = strip (Map.unionWith (+) y x)
   (-) x y = (+) x (negate y)
   {- won't work because Map.unionWith won't negate a value from y if no x value corresponds to it
   (-) x y = strip (Map.unionWith sub x y)
   -}
   negate  = fmap negate

instance Ord i => Vector.C (Map i) where
   zero  = Map.empty
   (<+>) = Map.unionWith (+)
   -- requires Eq instance for expo
   -- expo *> x = if expo == zero then zero else Vector.functorScale expo x
   (*>)  = Vector.functorScale

instance (Ord i, Eq a, Eq v, Module.C a v)
             => Module.C a (Map i v) where
--   (*>) 0    = \_ -> zero
--   (*>) expo = fmap ((*>) expo)
   (*>) expo x = if expo == zero then zero else fmap (expo *>) x

instance (Ord i, Eq a, Eq v, VectorSpace.C a v)
             => VectorSpace.C a (Map i v)

instance (Ord i, Eq a, Eq v, NormedSum.C a v)
             => NormedSum.C a (Map i v) where
   norm = foldl (+) zero . map NormedSum.norm . Map.elems

instance (Ord i, Eq a, Eq v, NormedEuc.Sqr a v)
             => NormedEuc.Sqr a (Map i v) where
   normSqr = foldl (+) zero . map NormedEuc.normSqr . Map.elems

instance (Ord i, Eq a, Eq v, Algebraic.C a, NormedEuc.Sqr a v)
             => NormedEuc.C a (Map i v) where
   norm = NormedEuc.defltNorm

instance (Ord i, Eq a, Eq v, NormedMax.C a v)
             => NormedMax.C a (Map i v) where
   norm = foldl max zero . map NormedMax.norm . Map.elems
