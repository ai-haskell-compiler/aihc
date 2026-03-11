-- |
-- Module      :  Tree
-- Description :  Benchmark some tree algorithms
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Jul  2 13:10:48 2021.
module Tree
  ( toLengthTreeTraversable,
    toLengthTreeBitraversable,
  )
where

import Data.Bifunctor
import Data.Bitraversable
import ELynx.Tree.Length
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted

-- Compare speed of bitraversal and special traversal instances.

toLengthTreeTraversable :: Tree Phylo a -> Tree Length a
toLengthTreeTraversable = either error id . toLengthTree

fromMaybeWithError :: String -> Maybe a -> Either String a
fromMaybeWithError s = maybe (Left s) Right

toLengthTreeBitraversable :: HasMaybeLength e => Tree e a -> Either String (Tree Length a)
toLengthTreeBitraversable t =
  fromMaybeWithError "toLengthTree: Length unavailable for some branches." $ bisequenceA t'
  where
    t' = modifyStem cleanLength $ bimap getMaybeLength pure t
    cleanLength Nothing = pure 0
    cleanLength x = x
