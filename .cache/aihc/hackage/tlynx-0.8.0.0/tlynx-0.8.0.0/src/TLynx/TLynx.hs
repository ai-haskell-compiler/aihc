-- |
-- Module      :  TLynx.TLynx
-- Description :  TLynx module
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Apr 23 16:36:43 2020.
module TLynx.TLynx
  ( tlynx,
    rTLynx,
  )
where

import ELynx.Tools.ELynx
import ELynx.Tools.Options
import TLynx.Compare.Compare
import TLynx.Connect.Connect
import TLynx.Distance.Distance
import TLynx.Examine.Examine
import TLynx.Options
import TLynx.Shuffle.Shuffle
import TLynx.Simulate.Simulate

-- | Run TLynx with given arguments.
tlynx :: Arguments CommandArguments -> IO ()
tlynx c = case local c of
  Compare l -> eLynxWrapper g l Compare compareCmd
  Connect l -> eLynxWrapper g l Connect connectCmd
  Distance l -> eLynxWrapper g l Distance distance
  Examine l -> eLynxWrapper g l Examine examine
  Shuffle l -> eLynxWrapper g l Shuffle shuffleCmd
  Simulate l -> eLynxWrapper g l Simulate simulate
  where
    g = global c

-- | Run TLynx, parse arguments from command line.
rTLynx :: IO ()
rTLynx = parseArguments >>= tlynx
