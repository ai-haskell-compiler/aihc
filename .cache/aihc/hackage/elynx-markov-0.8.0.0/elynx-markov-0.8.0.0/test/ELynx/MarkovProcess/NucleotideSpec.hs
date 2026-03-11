-- |
-- Module      :  ELynx.MarkovProcess.NucleotideSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 25 16:47:28 2019.
module ELynx.MarkovProcess.NucleotideSpec
  ( spec,
  )
where

import Data.Vector.Generic
import ELynx.MarkovProcess.Nucleotide
import ELynx.MarkovProcess.RateMatrix
import ELynx.MarkovProcess.SubstitutionModel
import ELynx.Tools.Equality
import Test.Hspec

stationaryDist :: StationaryDistribution
stationaryDist = fromList [0.2, 0.3, 0.3, 0.2]

hkyModel :: SubstitutionModel
hkyModel = hky DoNormalize 6.0 stationaryDist

spec :: Spec
spec =
  describe "getStationaryDistribution" $
    it "extracts the stationary distribution from a rate matrix" $
      do
        let sd = getStationaryDistribution (rateMatrix hkyModel)
        sd `nearlyEqVec` stationaryDist `shouldBe` True
