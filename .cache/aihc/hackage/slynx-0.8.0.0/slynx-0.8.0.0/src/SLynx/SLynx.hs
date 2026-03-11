-- |
-- Module      :  SLynx.SLynx
-- Description :  SLynx module
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Apr 23 16:38:55 2020.
module SLynx.SLynx
  ( slynx,
    rSLynx,
  )
where

import ELynx.Tools.ELynx
import ELynx.Tools.Options
import SLynx.Concatenate.Concatenate
import SLynx.Examine.Examine
import SLynx.Filter.Filter
import SLynx.Options
import SLynx.Simulate.Simulate
import SLynx.SubSample.SubSample
import SLynx.Translate.Translate

-- | Run SLynx with given arguments.
slynx :: Arguments CommandArguments -> IO ()
slynx c = case local c of
  Concatenate l -> eLynxWrapper g l Concatenate concatenateCmd
  Examine l -> eLynxWrapper g l Examine examineCmd
  FilterCols l -> eLynxWrapper g l FilterCols filterColsCmd
  FilterRows l -> eLynxWrapper g l FilterRows filterRowsCmd
  Simulate l -> eLynxWrapper g l Simulate simulateCmd
  SubSample l -> eLynxWrapper g l SubSample subSampleCmd
  Translate l -> eLynxWrapper g l Translate translateCmd
  where
    g = global c

-- | Run SLynx, parse arguments from command line.
rSLynx :: IO ()
rSLynx = parseArguments >>= slynx
