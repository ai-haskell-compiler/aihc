{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Length
-- Description :  Labels having a length
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 14:16:34 2019.
--
-- Non-negativity of lengths is not completely ensured. See the documentation of
-- 'Length'.
module ELynx.Tree.Length
  ( -- * Non-negative length
    Length (fromLength),
    toLength,
    toLengthUnsafe,
    HasMaybeLength (..),
    HasLength (..),
    height,
    rootHeight,

    -- * Functions on trees
    distancesOriginLeaves,
    totalBranchLength,
    normalizeBranchLengths,
    normalizeHeight,
    ultrametric,
    makeUltrametric,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Bifunctor
import Data.Default.Class
import Data.Foldable
import Data.Semigroup
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import GHC.Generics

-- | Non-negative length.
--
-- However, non-negativity is only checked with 'toLength', and negative values
-- can be obtained using the 'Num' and related instances.
--
-- Safe conversion is roughly 50 percent slower.
--
-- @
-- benchmarking length/length sum foldl' with safe conversion
-- time                 110.4 ms   (109.8 ms .. 111.0 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 110.2 ms   (110.0 ms .. 110.6 ms)
-- std dev              501.8 μs   (359.1 μs .. 730.0 μs)
--
-- benchmarking length/length sum foldl' num instance
-- time                 89.37 ms   (85.13 ms .. 94.27 ms)
--                      0.996 R²   (0.992 R² .. 1.000 R²)
-- mean                 86.53 ms   (85.63 ms .. 88.52 ms)
-- std dev              2.239 ms   (1.069 ms .. 3.421 ms)
--
-- benchmarking length/double sum foldl'
-- time                 85.47 ms   (84.88 ms .. 86.42 ms)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 85.56 ms   (85.26 ms .. 86.02 ms)
-- std dev              611.9 μs   (101.5 μs .. 851.7 μs)
-- @
newtype Length = Length {fromLength :: Double}
  deriving (Read, Show, Generic, NFData)
  deriving
    ( Default,
      Enum,
      Eq,
      Floating,
      Fractional,
      Num,
      Ord,
      Real,
      RealFloat,
      RealFrac
    )
    via Double
  deriving (Semigroup, Monoid) via Sum Double

instance Splittable Length where
  split = (/ 2.0)

instance ToJSON Length

instance FromJSON Length

instance HasMaybeLength Length where
  getMaybeLength = Just

instance HasLength Length where
  getLength = id
  setLength = const
  modifyLength f = f

-- | Return 'Left' if negative.
toLength :: Double -> Either String Length
toLength x
  | x < 0 = Left $ "Length is negative: " ++ show x ++ "."
  | otherwise = Right $ Length x

-- | Do not check if value is negative.
toLengthUnsafe :: Double -> Length
toLengthUnsafe = Length

-- | Class of data types that may have a length.
class HasMaybeLength e where
  getMaybeLength :: e -> Maybe Length

instance HasMaybeLength () where
  getMaybeLength = const Nothing

-- | Class of data types with measurable and modifiable length.
class HasMaybeLength e => HasLength e where
  getLength :: e -> Length
  setLength :: Length -> e -> e
  modifyLength :: (Length -> Length) -> e -> e

-- | The maximum distance between origin and leaves.
--
-- The height includes the branch length of the stem.
height :: HasLength e => Tree e a -> Length
height = maximum . distancesOriginLeaves

-- | The maximum distance between root node and leaves.
rootHeight :: HasLength e => Tree e a -> Length
rootHeight (Node _ _ []) = 0
rootHeight t = maximum $ concatMap distancesOriginLeaves (forest t)

-- | Distances from the origin of a tree to the leaves.
--
-- The distances include the branch length of the stem.
distancesOriginLeaves :: HasLength e => Tree e a -> [Length]
distancesOriginLeaves (Node br _ []) = [getLength br]
distancesOriginLeaves (Node br _ ts) = map (getLength br +) (concatMap distancesOriginLeaves ts)

-- | Total branch length of a tree.
totalBranchLength :: HasLength e => Tree e a -> Length
totalBranchLength = foldl' (+) 0 . fmap getLength . ZipBranchTree

-- | Normalize branch lengths so that the sum is 1.0.
normalizeBranchLengths :: HasLength e => Tree e a -> Tree e a
normalizeBranchLengths t = first (modifyLength (/ s)) t
  where
    s = totalBranchLength t

-- | Normalize height of tree to 1.0.
normalizeHeight :: HasLength e => Tree e a -> Tree e a
normalizeHeight t = first (modifyLength (/ h)) t
  where
    h = height t

eps :: Double
eps = 1e-12

allNearlyEqual :: [Length] -> Bool
allNearlyEqual [] = True
allNearlyEqual xs = all (\y -> eps > abs (fromLength $ x - y)) (tail xs)
  where
    x = head xs

-- | Check if a tree is ultrametric.
ultrametric :: HasLength e => Tree e a -> Bool
ultrametric = allNearlyEqual . distancesOriginLeaves

-- | Elongate terminal branches such that the tree becomes ultrametric.
makeUltrametric :: HasLength e => Tree e a -> Tree e a
makeUltrametric t = go 0 t
  where
    h = height t
    go :: HasLength e => Length -> Tree e a -> Tree e a
    go h' (Node br lb []) = let dh = h - h' - getLength br in Node (modifyLength (+ dh) br) lb []
    go h' (Node br lb ts) = let h'' = h' + getLength br in Node br lb $ map (go h'') ts
