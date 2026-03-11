{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
--   Module      :  ELynx.Tree.Distribution.BirthDeathCriticalNoTime
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
-- reconstructed trees under the birth and death process; critical birth and death
-- process with lambda=mu.
module ELynx.Tree.Distribution.BirthDeathCriticalNoTime
  ( BirthDeathCriticalNoTimeDistribution (..),
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
newtype BirthDeathCriticalNoTimeDistribution = BDCNTD
  { -- | Birth and death rate.
    bdcntdLa :: Rate
  }
  deriving (Eq, Typeable, Data, Generic)

instance D.Distribution BirthDeathCriticalNoTimeDistribution where
  cumulative = cumulative

-- | Cumulative distribution function section 2.1.2, second formula.
cumulative :: BirthDeathCriticalNoTimeDistribution -> Time -> Double
cumulative (BDCNTD l) x
  | x <= 0 = 0
  | otherwise = x * l / (1.0 + x * l)

instance D.ContDistr BirthDeathCriticalNoTimeDistribution where
  density = density
  quantile = quantile

-- | Density function section 2.1.2, first formula; t cancels out because it is
-- expected to be much larger than 1.0; because t \in [0, \infty].
density :: BirthDeathCriticalNoTimeDistribution -> Time -> Double
density (BDCNTD l) x
  | x < 0 = 0
  | otherwise = l / ((1.0 + x * l) ** 2)

-- | Inverted cumulative probability distribution 'cumulative'. See also
-- 'D.ContDistr'.
quantile :: BirthDeathCriticalNoTimeDistribution -> Double -> Time
quantile (BDCNTD l) p
  | p >= 0 && p <= 1 =
      p / (l - l * p)
  | otherwise =
      error $ "PointProcess.quantile: p must be in [0,1]. Got: " ++ show p ++ "."

instance D.ContGen BirthDeathCriticalNoTimeDistribution where
  genContVar = D.genContinuous
