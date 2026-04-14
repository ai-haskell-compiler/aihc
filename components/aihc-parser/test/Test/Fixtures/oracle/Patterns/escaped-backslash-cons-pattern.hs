{- ORACLE_TEST xfail reason="character literals like '\^\\' in case alternative right-hand sides are rejected after escaped-backslash cons patterns" -}
module X where

go xs = case xs of
  '^' : '\\' : xs -> '\^\' : go xs
  ys -> ys
