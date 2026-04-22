{- ORACLE_TEST xfail parser rejects prefix-section (:) constructor in case pattern -}
module A where

f xs = case xs of
  (:) x ys -> x
  [] -> error "empty"
