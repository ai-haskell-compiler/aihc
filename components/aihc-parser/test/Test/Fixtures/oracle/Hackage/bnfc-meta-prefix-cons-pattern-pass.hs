{- ORACLE_TEST pass -}
module A where

f xs = case xs of
  (:) x ys -> x
  [] -> error "empty"
