{-# LANGUAGE NoImplicitPrelude, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE CPP #-}

-- | 'Data.ListLike.ListLike' instances for 'Data.DList.DList'

module Data.ListLike.DList () where

import qualified Prelude

import Data.ListLike.Base
import Data.ListLike.FoldableLL
import Data.ListLike.String
#if MIN_VERSION_dlist(1,0,0)
import Data.DList.Unsafe (DList(UnsafeDList), unsafeApplyDList)
#else
import Data.DList (DList)
#endif
import qualified Data.DList as D
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.String as S
import Control.Category
import Data.Char (Char)


instance FoldableLL (DList a) a where
  foldl = F.foldl
  foldr = D.foldr
  foldl1 = F.foldl1
  foldr1 = F.foldr1
  foldl' = F.foldl'
  foldr' = F.foldr'

instance ListLike (DList a) a where
  empty = D.empty
  singleton = D.singleton
  cons = D.cons
  snoc = D.snoc
  append = D.append
  head = D.head
#if MIN_VERSION_dlist(1,0,0)
  -- Andreas Abel, 2021-09-01, issue #14,
  -- work around https://github.com/spl/dlist/issues/98:
  --
  -- dlist-1.0 changed @tail@ so that it is not an operation
  -- on difference lists anymore, but collapses the difference list
  -- into a plain list.
  --
  -- The following tail function restores the spirit of difference
  -- lists, at the cost of breaking data abstraction, i.e.,
  -- using the constructor and destructor of the newtype DList.
  -- Andreas Abel, 2023-10-10, issue #32:
  -- Use @drop 1@ instead of @tail@ as the latter triggers the x-partial warning in GHC 9.8.
  tail = UnsafeDList . (List.drop 1 .) . unsafeApplyDList
#else
  tail = D.tail
#endif
  rigidMap = D.map
  null = null . D.toList
  --toList = D.toList
  --fromList = D.fromList
  replicate = D.replicate
  uncons xs = case xs of
    D.Nil -> Prelude.Nothing
    D.Cons d_head l_tail -> Prelude.Just (d_head,fromList l_tail)
    _ -> Prelude.error "Workaround for missing COMPLETE pragma on dlist patterns"


instance StringLike (DList Char) where
  toString = D.toList
  -- fromString = D.fromList
  lines = map D.fromList . S.lines . D.toList
  words = map D.fromList . S.words . D.toList
  unlines = D.fromList . S.unlines . map D.toList
  unwords = D.fromList . S.unwords . map D.toList
