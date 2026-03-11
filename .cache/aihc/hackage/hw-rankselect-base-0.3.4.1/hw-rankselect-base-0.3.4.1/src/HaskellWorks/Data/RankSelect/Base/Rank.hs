{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.RankSelect.Base.Rank
  ( -- * Rank & Select
    Rank(..)
  ) where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank0
import HaskellWorks.Data.RankSelect.Base.Rank1

class Eq a => Rank v a where
  -- | Find the number of occurences of the given symbol in the prefix of the supplied bitstring of the given length
  rank
    :: a      -- ^ The symbol
    -> v      -- ^ The bitstring
    -> Count  -- ^ The prefix length
    -> Count

instance Rank [Bool] Bool where
  rank a = if a then rank1 else rank0
  {-# INLINABLE rank #-}
