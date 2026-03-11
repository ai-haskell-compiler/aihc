{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Module      :  ELynx.Tree.Support
-- Description :  Labels with support values
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 13 14:06:45 2019.
module ELynx.Tree.Support
  ( -- * Non-negative support value
    Support (fromSupport),
    toSupport,
    toSupportUnsafe,
    HasMaybeSupport (..),
    HasSupport (..),

    -- * Functions on trees
    normalizeBranchSupport,
    collapse,
  )
where

import Control.DeepSeq
import Data.Aeson
import Data.Bifunctor
import Data.List
import Data.Semigroup
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import GHC.Generics

-- | Non-negative support value.
--
-- However, non-negativity is only checked with 'toSupport', and negative values
-- can be obtained using the 'Num' and related instances.
--
-- See also the documentation of 'ELynx.Tree.Length.Length'.
newtype Support = Support {fromSupport :: Double}
  deriving (Read, Show, Generic, NFData)
  deriving (Enum, Eq, Floating, Fractional, Num, Ord, Real, RealFloat, RealFrac) via Double
  deriving (Semigroup) via Min Double

instance Splittable Support where
  split = id

instance ToJSON Support

instance FromJSON Support

instance HasMaybeSupport Support where
  getMaybeSupport = Just

instance HasMaybeSupport () where
  getMaybeSupport = const Nothing

instance HasSupport Support where
  getSupport = id
  setSupport = const
  modifySupport f = f

-- | Return 'Left' if negative.
toSupport :: Double -> Either String Support
toSupport x
  | x < 0 = Left $ "Support is negative: " ++ show x ++ "."
  | otherwise = Right $ Support x

-- | Do not check if value is negative.
toSupportUnsafe :: Double -> Support
toSupportUnsafe = Support

-- | Class of data types that may have a support value.
class HasMaybeSupport e where
  getMaybeSupport :: e -> Maybe Support

-- | Class of data types with measurable and modifiable support values.
class HasMaybeSupport e => HasSupport e where
  getSupport :: e -> Support
  setSupport :: Support -> e -> e
  modifySupport :: (Support -> Support) -> e -> e

-- | Normalize branch support values. The maximum branch support value will be
-- set to 1.0.
normalizeBranchSupport :: HasSupport e => Tree e a -> Tree e a
normalizeBranchSupport t = first (modifySupport (/ m)) t
  where
    m = maximum $ getSupport <$> ZipBranchTree t

-- | Collapse branches with support lower than given value.
--
-- The branch and node labels of the collapsed branches are discarded.
collapse :: (Eq e, Eq a, HasSupport e) => Support -> Tree e a -> Tree e a
collapse th tr =
  let tr' = collapse' th tr
   in if tr == tr' then tr else collapse th tr'

-- A leaf has full support.
highP :: HasSupport e => Support -> Tree e a -> Bool
highP _ (Node _ _ []) = True
highP th (Node br _ _) = getSupport br >= th

-- See 'collapse'.
collapse' :: HasSupport e => Support -> Tree e a -> Tree e a
collapse' th (Node br lb ts) = Node br lb $ map (collapse' th) (highSupport ++ lowSupportForest)
  where
    (highSupport, lowSupport) = partition (highP th) ts
    lowSupportForest = concatMap forest lowSupport
