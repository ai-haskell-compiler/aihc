{-# LANGUAGE RebindableSyntax #-}
{- |
Abstract Physical Units
-}

module Number.Physical.Unit where

import MathObj.DiscreteMap (strip)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe(fromJust,fromMaybe)

import qualified Number.Ratio as Ratio

import Data.Maybe.HT(toMaybe)

import NumericPrelude.Base
import NumericPrelude.Numeric

{- | A Unit.T is a sparse vector with integer entries
   Each map n->m means that the unit of the n-th dimension
   is given m times.

   Example: Let the quantity of length (meter, m) be the zeroth dimension
   and let the quantity of time (second, s) be the first dimension,
   then the composed unit @m/s^2@ corresponds to the Map
   @[(0,1),(1,-2)]@.

   In future I want to have more abstraction here,
   e.g. a type class from the Edison project
   that abstracts from the underlying implementation.
   Then one can easily switch between
   Arrays, Binary trees (like Map) and what know I.
-}
type T i = Map i Int

-- | The neutral Unit.T
scalar :: T i
scalar = Map.empty

-- | Test for the neutral Unit.T
isScalar ::  T i -> Bool
isScalar = Map.null

-- | Convert a List to sparse Map representation
-- Example: [-1,0,-2] -> [(0,-1),(2,-2)]
fromVector :: (Enum i, Ord i) => [Int] -> T i
fromVector x = strip (Map.fromList (zip [toEnum 0 .. toEnum ((length x)-1)] x))

-- | Convert Map to a List
toVector :: (Enum i, Ord i) => T i -> [Int]
toVector x = map (flip (Map.findWithDefault 0) x)
                     [(toEnum 0)..(maximum (Map.keys x))]


ratScale :: Ratio.T Int -> T i -> T i
ratScale expo =
   fmap (fromMaybe (error "Physics.Quantity.Unit.ratScale: fractional result")) .
   ratScaleMaybe2 expo

ratScaleMaybe :: Ratio.T Int -> T i -> Maybe (T i)
ratScaleMaybe expo u =
   let fmMaybe = ratScaleMaybe2 expo u
   in  toMaybe (not (Nothing `elem` Map.elems fmMaybe))
               (fmap fromJust fmMaybe)

-- helper function for ratScale and ratScaleMaybe
ratScaleMaybe2 :: Ratio.T Int -> T i -> Map i (Maybe Int)
ratScaleMaybe2 expo =
   fmap (\c -> let y = Ratio.scale c expo
               in  toMaybe (denominator y == 1) (numerator y))


{- impossible because Unit.T is a type synonym but not a data type
instance Show (Unit.T i) where
  show = show.toVector
-}
