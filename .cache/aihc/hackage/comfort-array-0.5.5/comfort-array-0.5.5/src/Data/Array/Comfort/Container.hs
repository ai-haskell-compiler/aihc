{-# LANGUAGE TypeFamilies #-}
{- |
This module provides an array shape type,
that allows to store elements from a container
while preserving the container structure.
-}
module Data.Array.Comfort.Container (
   C(..), EqShape(..), NFShape(..), Indexed(..),
   ) where

import qualified Data.Array.Comfort.Shape as Shape

import Control.DeepSeq (NFData, rnf)

import qualified Data.NonEmpty.Map as NonEmptyMap
import qualified Data.NonEmpty.Set as NonEmptySet
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Data.Complex as Complex
import Data.Map (Map)
import Data.Set (Set)
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)
-- import Data.Complex (Complex((:+)))



class (Foldable f) => C f where
   data Shape f
   shapeSize :: Shape f -> Int
   fromList :: Shape f -> [a] -> f a
   toShape :: f a -> Shape f

class (C f) => NFShape f where
   rnfShape :: Shape f -> ()

class (C f) => EqShape f where
   eqShape :: Shape f -> Shape f -> Bool

class (C f) => Indexed f where
   type Index f
   indices :: Shape f -> [Index f]
   unifiedSizeOffset ::
      (Shape.Checking check) =>
      Shape f -> (Int, Index f -> Shape.Result check Int)


instance (NFShape f) => NFData (Shape f) where
   rnf = rnfShape

instance (EqShape f) => Eq (Shape f) where
   (==) = eqShape

instance (C f) => Shape.C (Shape f) where
   size = shapeSize

instance (Indexed f) => Shape.Indexed (Shape f) where
   type Index (Shape f) = Index f
   indices = indices
   unifiedSizeOffset = unifiedSizeOffset


instance C [] where
   data Shape [] = ShapeList Int
      deriving (Show)
   shapeSize (ShapeList n) = n
   toShape = ShapeList . length
   fromList _ = id

instance EqShape [] where
   eqShape (ShapeList n)  (ShapeList m) = n==m

instance NFShape [] where
   rnfShape (ShapeList n) = rnf n

instance Indexed [] where
   type Index [] = Int
   indices (ShapeList len) = take len $ iterate (1+) 0
   unifiedSizeOffset (ShapeList len) =
      (len, \ix -> do
         Shape.assert "Shape.Container.[]: array index too small" $ ix>=0
         Shape.assert "Shape.Container.[]: array index too big" $ ix<len
         return ix)


{-
instance Foldable only available since GHC-8.0. :-(
Could be circumvented by Data.Orphans
but that one also pulls in lots of dangerous instances.

instance C Complex where
   data Shape Complex = ShapeComplex
   shapeSize ShapeComplex = 2
   toShape (_:+_) = ShapeComplex
   fromList ShapeComplex xs =
      case xs of
         [r,i] -> r Complex.:+ i
         _ -> error "ShapeComplex: not two elements"

instance EqShape Complex where
   eqShape ShapeComplex ShapeComplex = True

instance NFShape Complex where
   rnfShape ShapeComplex = ()
-}


instance (C f) => C (NonEmpty.T f) where
   data Shape (NonEmpty.T f) = ShapeNonEmpty (Shape f)
   shapeSize (ShapeNonEmpty c) = 1 + shapeSize c
   toShape = ShapeNonEmpty . toShape . NonEmpty.tail
   fromList (ShapeNonEmpty c) xt =
      case xt of
         [] -> error "ShapeNonEmpty: empty list"
         x:xs -> NonEmpty.cons x $ fromList c xs

instance (EqShape f) => EqShape (NonEmpty.T f) where
   eqShape (ShapeNonEmpty a) (ShapeNonEmpty b) = a==b

instance (NFShape f) => NFShape (NonEmpty.T f) where
   rnfShape (ShapeNonEmpty c) = rnfShape c

instance (C f) => Indexed (NonEmpty.T f) where
   type Index (NonEmpty.T f) = Int
   indices shape = take (shapeSize shape) $ iterate (1+) 0
   unifiedSizeOffset shape =
      let len = shapeSize shape in
      (len, \ix -> do
         Shape.assert "Shape.Container.NonEmpty: array index too small" $ ix>=0
         Shape.assert "Shape.Container.NonEmpty: array index too big" $ ix<len
         return ix)


instance C Empty.T where
   data Shape Empty.T = ShapeEmpty
      deriving (Show)
   shapeSize ShapeEmpty = 0
   toShape Empty.Cons = ShapeEmpty
   fromList ShapeEmpty xs =
      case xs of
         [] -> Empty.Cons
         _ -> error "ShapeEmpty: not empty"

instance EqShape Empty.T where
   eqShape ShapeEmpty ShapeEmpty = True

instance NFShape Empty.T where
   rnfShape ShapeEmpty = ()


instance (Ord k) => C (Map k) where
   data Shape (Map k) = ShapeMap (Set k)
      deriving (Show)
   shapeSize (ShapeMap set) = Set.size set
   toShape = ShapeMap . Map.keysSet
   fromList (ShapeMap set) = Map.fromAscList . zip (Set.toAscList set)

instance (Ord k) => EqShape (Map k) where
   eqShape (ShapeMap set0) (ShapeMap set1) = set0==set1

instance (NFData k, Ord k) => NFShape (Map k) where
   rnfShape (ShapeMap set) = rnf set

instance (Ord k) => Indexed (Map k) where
   type Index (Map k) = k
   indices (ShapeMap set) = Set.toAscList set
   unifiedSizeOffset (ShapeMap set) = Shape.unifiedSizeOffset set


instance (Ord k) => C (NonEmptyMap.T k) where
   data Shape (NonEmptyMap.T k) = ShapeNonEmptyMap (NonEmptySet.T k)
      deriving (Show)
   shapeSize (ShapeNonEmptyMap set) = NonEmptySet.size set
   toShape = ShapeNonEmptyMap . NonEmptyMap.keysSet
   fromList (ShapeNonEmptyMap set) =
      NonEmptyMap.fromAscList . NonEmptyC.zip (NonEmptySet.toAscList set) .
      fromMaybe (error "ShapeNonEmptyMap: empty list") . NonEmpty.fetch

instance (Ord k) => EqShape (NonEmptyMap.T k) where
   eqShape (ShapeNonEmptyMap set0) (ShapeNonEmptyMap set1) = set0==set1

instance (NFData k, Ord k) => NFShape (NonEmptyMap.T k) where
   rnfShape (ShapeNonEmptyMap set) = rnf set

instance (Ord k) => Indexed (NonEmptyMap.T k) where
   type Index (NonEmptyMap.T k) = k
   indices (ShapeNonEmptyMap set) =
      NonEmpty.flatten $ NonEmptySet.toAscList set
   unifiedSizeOffset (ShapeNonEmptyMap set) =
      Shape.unifiedSizeOffset (NonEmptySet.flatten set)
