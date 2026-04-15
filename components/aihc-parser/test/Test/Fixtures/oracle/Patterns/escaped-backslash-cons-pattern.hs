{- ORACLE_TEST pass -}
module X where

go xs = case xs of
  '^' : '\\' : xs -> '\^\' : go xs
  ys -> ys
