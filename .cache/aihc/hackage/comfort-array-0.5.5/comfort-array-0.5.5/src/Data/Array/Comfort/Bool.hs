{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
Can be an alternative to the @enumset@ package.
-}
module Data.Array.Comfort.Bool (
   Array,
   shape,
   reshape,
   mapShape,

   fromList,
   toList,
   fromSet,
   toSet,

   member,
   union,
   difference,
   intersection,
   ) where

import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.Array.Comfort.Check as Check

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.List as List
import Data.IntSet (IntSet)
import Data.Set (Set)


data Array sh =
   Array {
      shape_ :: sh,
      _intSet :: IntSet
   }


shape :: Array sh -> sh
shape = shape_

reshape :: (Shape.C sh0, Shape.C sh1) => sh1 -> Array sh0 -> Array sh1
reshape = Check.reshape "Storable" shape (\sh arr -> arr{shape_ = sh})

mapShape ::
   (Shape.C sh0, Shape.C sh1) => (sh0 -> sh1) -> Array sh0 -> Array sh1
mapShape f arr = reshape (f $ shape arr) arr



fromList :: (Shape.Indexed sh) => sh -> [Shape.Index sh] -> Array sh
fromList sh = Array sh . IntSet.fromList . List.map (Shape.offset sh)

toList :: (Shape.InvIndexed sh) => Array sh -> [Shape.Index sh]
toList (Array sh set) = map (Shape.indexFromOffset sh) $ IntSet.toList set

fromSet ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Ord ix) => sh -> Set ix -> Array sh
fromSet sh = fromList sh . Set.toList

toSet ::
   (Shape.InvIndexed sh, Shape.Index sh ~ ix, Ord ix) => Array sh -> Set ix
toSet = Set.fromList . toList


errorArray :: String -> String -> a
errorArray name msg =
   error ("Array.Comfort.Bool." ++ name ++ ": " ++ msg)


member :: (Shape.Indexed sh) => Shape.Index sh -> Array sh -> Bool
member ix (Array sh set) = IntSet.member (Shape.offset sh ix) set


lift2 :: (Shape.Indexed sh, Eq sh) =>
   String -> (IntSet -> IntSet -> IntSet) ->
   Array sh -> Array sh -> Array sh
lift2 name op (Array shA setA) (Array shB setB) =
   if shA == shB
      then Array shA $ op setA setB
      else errorArray name "shapes mismatch"

union :: (Shape.Indexed sh, Eq sh) => Array sh -> Array sh -> Array sh
union = lift2 "union" IntSet.union

intersection :: (Shape.Indexed sh, Eq sh) => Array sh -> Array sh -> Array sh
intersection = lift2 "intersection" IntSet.intersection

difference :: (Shape.Indexed sh, Eq sh) => Array sh -> Array sh -> Array sh
difference = lift2 "difference" IntSet.difference
