{- ORACLE_TEST pass -}
module A where
f = g
  where
    g = {-# SCC "name" #-} undefined
