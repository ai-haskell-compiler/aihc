-- |
-- Module      :  ELynx.Import.MarkovProcess.SiteprofilesPhylobayesSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed Sep 18 09:51:35 2019.
module ELynx.Import.MarkovProcess.SiteprofilesPhylobayesSpec
  ( spec,
  )
where

import qualified Data.Vector.Storable as V
import ELynx.Import.MarkovProcess.EDMModelPhylobayes
import ELynx.Import.MarkovProcess.SiteprofilesPhylobayes
import ELynx.Tools.Equality
import ELynx.Tools.InputOutput
import Test.Hspec

fn :: FilePath
fn = "data/HSSPMany.siteprofiles"

getProfiles :: IO [EDMComponent]
getProfiles = parseFileWith siteprofiles fn

firstProfile :: V.Vector Double
firstProfile =
  V.fromList
    [ 0.0267009,
      0.013874,
      0.022432,
      0.0440502,
      0.0485174,
      0.0407515,
      0.0170806,
      0.348043,
      0.0371379,
      0.0715536,
      0.0454168,
      0.0342213,
      0.0146872,
      0.0239681,
      0.0768379,
      0.0210387,
      0.0123336,
      0.0512678,
      0.0149093,
      0.0351776
    ]

spec :: Spec
spec =
  describe "import phylobayes siteprofiles" $
    it "parses a text file with siteprofiles in phylobayes format" $
      do
        profiles <- getProfiles
        length profiles `shouldBe` 701
        map fst profiles `shouldBe` replicate 701 1.0
        map (V.sum . snd) profiles
          `shouldSatisfy` nearlyEqListWith 1e-5 (replicate 701 1.0)
        snd (head profiles) `shouldBe` firstProfile
