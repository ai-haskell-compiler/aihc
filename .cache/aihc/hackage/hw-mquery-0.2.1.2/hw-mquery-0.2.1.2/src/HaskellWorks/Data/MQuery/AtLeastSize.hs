
module HaskellWorks.Data.MQuery.AtLeastSize where

class AtLeastSize a where
  atLeastSize :: a -> Int -> Bool

instance AtLeastSize [a] where
  atLeastSize  _      0 = True
  atLeastSize (_:as)  n | n > 0 = atLeastSize as (n - 1)
  atLeastSize  _      _ = False
