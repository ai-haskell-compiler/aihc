{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      :  ELynx.Tree.Export.Newick
-- Description :  Export tree objects to Newick format
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jan 17 13:51:47 2019.
--
-- Some functions are inspired by
-- [Biobase.Newick.Import](https://hackage.haskell.org/package/BiobaseNewick).
--
-- See nomenclature in 'ELynx.Tree'.
module ELynx.Tree.Export.Newick
  ( toNewick,
    toNewickBuilder,
  )
where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (intersperse)
import ELynx.Tree.Length
import ELynx.Tree.Name
import ELynx.Tree.Phylogeny
import ELynx.Tree.Rooted
import ELynx.Tree.Support

-- Allow export of trees having branches with lengths only.
instance HasMaybeSupport Length where
  getMaybeSupport = const Nothing

-- Allow export of trees having branches with support values only.
instance HasMaybeLength Support where
  getMaybeLength = const Nothing

buildBrLen :: Length -> BB.Builder
buildBrLen bl = BB.char8 ':' <> BB.doubleDec (fromLength bl)

buildBrSup :: Support -> BB.Builder
buildBrSup bs = BB.char8 '[' <> BB.doubleDec (fromSupport bs) <> BB.char8 ']'

-- | See 'toNewick'.
toNewickBuilder :: (HasMaybeLength e, HasMaybeSupport e, HasName a) => Tree e a -> BB.Builder
toNewickBuilder t = go t <> BB.char8 ';'
  where
    go (Node b l []) = lbl b l
    go (Node b l ts) =
      BB.char8 '('
        <> mconcat (intersperse (BB.char8 ',') $ map go ts)
        <> BB.char8 ')'
        <> lbl b l
    mBrSupBuilder x = maybe mempty buildBrSup (getMaybeSupport x)
    mBrLenBuilder x = maybe mempty buildBrLen (getMaybeLength x)
    lbl x y =
      BB.lazyByteString (fromName $ getName y)
        <> mBrLenBuilder x
        -- After reading several discussions, I go for the "more semantical
        -- form" with branch support values in square brackets.
        <> mBrSupBuilder x
{-# SPECIALIZE toNewickBuilder :: Tree Length Name -> BB.Builder #-}
{-# SPECIALIZE toNewickBuilder :: Tree Length Int -> BB.Builder #-}
{-# SPECIALIZE toNewickBuilder :: Tree Phylo Name -> BB.Builder #-}
{-# SPECIALIZE toNewickBuilder :: Tree Phylo Int -> BB.Builder #-}

-- | General conversion of a tree into a Newick 'BL.ByteString'.
--
-- Functions to write key value pairs for nodes are not provided. Those can just
-- be set as node labels. For example, the posterior density and the confidence
-- interval of a node can be encoded by setting the node label to a
-- 'BL.ByteString':
--
-- @
-- "ACTUALNAME[posterior=-2839.2,age_95%_HPD={4.80804,31.6041}]"
-- @
toNewick :: (HasMaybeLength e, HasMaybeSupport e, HasName a) => Tree e a -> BL.ByteString
toNewick = BB.toLazyByteString . toNewickBuilder
{-# SPECIALIZE toNewick :: Tree Length Name -> BL.ByteString #-}
{-# SPECIALIZE toNewick :: Tree Length Int -> BL.ByteString #-}
{-# SPECIALIZE toNewick :: Tree Phylo Name -> BL.ByteString #-}
{-# SPECIALIZE toNewick :: Tree Phylo Int -> BL.ByteString #-}
