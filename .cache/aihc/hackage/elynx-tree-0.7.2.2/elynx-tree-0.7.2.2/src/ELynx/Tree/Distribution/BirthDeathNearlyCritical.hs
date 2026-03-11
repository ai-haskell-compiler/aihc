{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
--   Module      :  ELynx.Tree.Distribution.BirthDeathNearlyCritical
--   Description :  Birth and death distribution
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
-- Distribution of the values of the point process such that it corresponds to
-- reconstructed trees under the birth and death process; nearly critical birth and
-- death process with lambda~mu.
--
-- Basically, this is a Taylor expansion of Eq. (2) and Eq. (3).
module ELynx.Tree.Distribution.BirthDeathNearlyCritical
  ( BirthDeathNearlyCriticalDistribution (..),
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

-- | Distribution of the values of the point process such that it corresponds to
-- a reconstructed tree of the birth and death process.
data BirthDeathNearlyCriticalDistribution = BDNCD
  { -- | Time to origin of the tree.
    bdncdTOr :: Time,
    -- | Birth and death rate.
    bdncdLa :: Rate,
    -- | Birth and death rate.
    bdncdMu :: Rate
  }
  deriving (Eq, Typeable, Data, Generic)

instance D.Distribution BirthDeathNearlyCriticalDistribution where
  cumulative = cumulative

-- | Cumulative distribution function section 2.1.2, second formula.
cumulative :: BirthDeathNearlyCriticalDistribution -> Time -> Double
cumulative (BDNCD t l m) s
  | s <= 0 = 0
  | s > t = 1
  | otherwise = o0 + o1
  where
    o0 = s * (1.0 + t * l) / t / (1.0 + s * l)
    o1 = (-s * s + s * t) * (m - l) / (2.0 * t * (1.0 + s * l) ** 2)

instance D.ContDistr BirthDeathNearlyCriticalDistribution where
  density = density
  quantile = quantile

-- | Density function section 2.1.2, first formula.
density :: BirthDeathNearlyCriticalDistribution -> Time -> Double
density (BDNCD t l m) s
  | s < 0 = 0
  | s > t = 0
  | otherwise = o0 + o1
  where
    o0 = (1.0 + t * l) / (t * (1.0 + s * l) ** 2)
    o1 = (-2.0 * s + t - s * t * l) * (m - l) / (2.0 * t * (1.0 + s * l) ** 3)

-- | Inverted cumulative probability distribution 'cumulative'. See also
-- 'D.ContDistr'.
quantile :: BirthDeathNearlyCriticalDistribution -> Double -> Time
quantile (BDNCD t l m) p
  | p >= 0 && p <= 1 =
      res
  | otherwise =
      error $
        "PointProcess.quantile: p must be in [0,1] range. Got: "
          ++ show p
          ++ "."
  where
    den = l * (-3.0 + 2.0 * t * (-1.0 + p) * l) + m
    t1 = (2.0 + t * (l - 4.0 * p * l + m)) / den
    t2Nom =
      4.0
        + t
          * ( l
                * (4.0 + t * l + 8.0 * p * (1.0 + t * l))
                + 2.0
                  * (2.0 + t * l - 4.0 * p * (1.0 + t * l))
                  * m
                + t
                  * m
                  * m
            )
    t2 = t2Nom / (den ** 2)
    res = 0.5 * (t1 + sqrt t2)

instance D.ContGen BirthDeathNearlyCriticalDistribution where
  genContVar = D.genContinuous
