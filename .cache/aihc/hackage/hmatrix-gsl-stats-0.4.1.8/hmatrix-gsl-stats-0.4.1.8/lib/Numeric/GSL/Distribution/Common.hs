-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Distribution.Common
-- Copyright   :  (c) A. V. H. McPhail 2010, 2015
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL common data types for distributions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-- FOr information on how to set environment variables to set RNG type and seed
-- see <http://www.gnu.org/software/gsl/manual/html_node/Random-number-environment-variables.html#Random-number-environment-variables>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Distribution.Common (
                                DistFunc(..)
                                ,RNG()
                                ,newRNG,seedRNG
                             ) where

-----------------------------------------------------------------------------

import Numeric.GSL.Distribution.Internal

-----------------------------------------------------------------------------

data DistFunc = Density    -- ^ pdf
              | Lower      -- ^ lower cdf
              | Upper      -- ^ upper cdf
              | LowInv     -- ^ lower inverse cdf
              | UppInv     -- ^ upper inverse cdf
                deriving(Enum,Eq)

-----------------------------------------------------------------------------

