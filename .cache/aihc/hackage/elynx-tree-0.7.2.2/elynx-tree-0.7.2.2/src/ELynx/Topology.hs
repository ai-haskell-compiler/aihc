-- |
-- Module      :  ELynx.Topology
-- Description :  Rooted topologies
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Oct 27 14:58:25 2020.
--
-- Convenience module combining all topology modules.
module ELynx.Topology
  ( -- * Rooted topologies
    module ELynx.Topology.Rooted,

    -- * Phylogenetics
    module ELynx.Topology.Phylogeny,
  )
where

import ELynx.Topology.Phylogeny
import ELynx.Topology.Rooted
