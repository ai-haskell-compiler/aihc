-- |
-- Module      :  ELynx.Tree.Distribution.CoalescentContinuous
-- Description :  Distribution of coalescent times
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed May 16 12:40:45 2018.
module ELynx.Tree.Distribution.CoalescentContinuous
  ( coalescentDistributionCont,
  )
where

import Numeric.SpecFunctions (choose)
import Statistics.Distribution.Exponential

-- | Distribution of the next coalescent event for a number of samples @n@. The
-- time is measured in units of effective number of population size.
coalescentDistributionCont ::
  -- | Sample size.
  Int ->
  ExponentialDistribution
coalescentDistributionCont n = exponential (choose n 2)
