{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
--   Module      :  ELynx.Tree.Distribution.TimeOfOriginNearCritical
--   Description :  Distribution of time of origin for birth and death trees
--   Copyright   :  2021 Dominik Schrempf
--   License     :  GPL-3.0-or-later
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  portable
--
-- Creation date: Tue Feb 13 13:16:18 2018.
--
-- See Gernhard, T. (2008). The conditioned reconstructed process. Journal of
-- Theoretical Biology, 253(4), 769–778. http://doi.org/10.1016/j.jtbi.2008.04.005.
--
-- Distribution of the time of origin for birth and death trees. See corollary 3.3
-- in the paper cited above.
module ELynx.Tree.Distribution.TimeOfOriginNearCritical
  ( TimeOfOriginNearCriticalDistribution (..),
    cumulative,
    density,
    quantile,
  )
where

import Data.Data
  ( Data,
    Typeable,
  )
import ELynx.Tree.Distribution.Types
import GHC.Generics (Generic)
import qualified Statistics.Distribution as D

-- | Distribution of the time of origin for a phylogenetic tree evolving under
-- the birth and death process and conditioned on observing n leaves today.
data TimeOfOriginNearCriticalDistribution = TONCD
  { -- | Number of leaves of the tree.
    todTN :: Int,
    -- | Birth rate.
    todLa :: Rate,
    -- | Death rate.
    todMu :: Rate
  }
  deriving (Eq, Typeable, Data, Generic)

instance D.Distribution TimeOfOriginNearCriticalDistribution where
  cumulative = cumulative

-- | Cumulative distribution function; see Mathematica notebook.
cumulative :: TimeOfOriginNearCriticalDistribution -> Time -> Double
cumulative (TONCD n' l m) t
  | t <= 0 = 0
  | otherwise = t1 + t2
  where
    d = l - m
    n = fromIntegral n'
    t1 = (t * l / (1.0 + t * l)) ** n
    t2 = (n * t * t1) * d / (2.0 * (1.0 + t * l))

instance D.ContDistr TimeOfOriginNearCriticalDistribution where
  density = density
  quantile = quantile

-- | The density function Eq. (5).
density :: TimeOfOriginNearCriticalDistribution -> Time -> Double
density (TONCD n' l m) t
  | t < 0 = 0
  | otherwise = nom / den
  where
    n = fromIntegral n'
    nom =
      n * (t * l / (1 + t * l)) ** n * (2 + (3 + n) * t * l - (1 + n) * t * m)
    den = 2 * t * (1 + t * l) ** 2

-- | The inverted cumulative probability distribution 'cumulative'. See also
-- 'D.ContDistr'.
quantile :: TimeOfOriginNearCriticalDistribution -> Double -> Time
quantile (TONCD n' l m) p
  | p >= 0 && p <= 1 =
      t1 + t2nom / t2den
  | otherwise =
      error $
        "PointProcess.quantile: p must be in [0,1] range. Got: "
          ++ show p
          ++ "."
  where
    n = fromIntegral n'
    t1 = -p ** (1 / n) / ((-1 + p ** (1 / n)) * l)
    t2nom = p ** (2 / n) * (m - l)
    t2den = 2 * (-1 + p ** (1 / n)) ** 2 * l ** 2

instance D.ContGen TimeOfOriginNearCriticalDistribution where
  genContVar = D.genContinuous
