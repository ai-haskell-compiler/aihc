{- ORACLE_TEST pass -}
module GhcEventsConcatIfChainRoundtripXfail where

f a b c d e =
  "cost centre " <> a
  <> " " <> b
  <> " in " <> c
  <> " at " <> d
  <> if e then " CAF" else ""
