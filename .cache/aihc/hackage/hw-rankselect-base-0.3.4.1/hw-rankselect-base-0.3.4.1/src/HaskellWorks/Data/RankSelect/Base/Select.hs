{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.RankSelect.Base.Select
    ( -- * Rank & Select
      Select(..)
    ) where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Select0
import HaskellWorks.Data.RankSelect.Base.Select1

class Eq a => Select v a where
  -- | Find length of the shortest prefix of the given prefix that contains specified number of occurences of the given symbol
  select
    :: a      -- ^ The symbol
    -> v      -- ^ The bitstring
    -> Count  -- ^ The number of occurences
    -> Count

instance Select [Bool] Bool where
  select a = if a then select1 else select0
  {-# INLINABLE select #-}
