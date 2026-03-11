-- |
-- Module      :  ELynx.Tree
-- Description :  Rooted trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Mar 21 16:27:20 2020.
--
-- Convenience module combining all tree modules.
module ELynx.Tree
  ( -- * Rooted trees
    module ELynx.Tree.Rooted,

    -- * Tree zipper
    module ELynx.Tree.Zipper,

    -- * Parallel evaluation
    module ELynx.Tree.Parallel,

    -- * Branch label classes
    module ELynx.Tree.Length,
    module ELynx.Tree.Splittable,
    module ELynx.Tree.Support,

    -- * Node label classes
    module ELynx.Tree.Name,

    -- * Phylogenies
    module ELynx.Tree.Phylogeny,

    -- * Most recent common ancestors
    module ELynx.Tree.Mrca,

    -- * Partitions and distances
    module ELynx.Tree.Bipartition,
    module ELynx.Tree.Partition,
    module ELynx.Tree.Distance,

    -- * Import and Export
    module ELynx.Tree.Export.Newick,
    module ELynx.Tree.Export.Nexus,
    module ELynx.Tree.Import.Newick,
    module ELynx.Tree.Import.Nexus,
  )
where

import ELynx.Tree.Bipartition
import ELynx.Tree.Distance
import ELynx.Tree.Export.Newick
import ELynx.Tree.Export.Nexus
import ELynx.Tree.Import.Newick
import ELynx.Tree.Import.Nexus
import ELynx.Tree.Length
import ELynx.Tree.Mrca
import ELynx.Tree.Name
import ELynx.Tree.Parallel
import ELynx.Tree.Partition
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted
import ELynx.Tree.Splittable
import ELynx.Tree.Support
import ELynx.Tree.Zipper

-- -- | An evolutionary label has some information about where the corresponding
-- -- node is on the tree, and if the node is 'extant', 'extinct', 'internal', or
-- -- 'external'. The latter two could also be determined from the tree. This could
-- -- be species, genes or individuals; probably more.
-- class EvoLabel n where
--   extant          :: n -> Bool
--   extinct         :: n -> Bool

--   internal        :: n -> Bool
--   internal n = not $ extant n || extinct n

--   external        :: n -> Bool
--   external   = not . internal
