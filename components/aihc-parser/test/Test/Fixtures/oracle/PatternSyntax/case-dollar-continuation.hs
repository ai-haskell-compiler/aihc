{- ORACLE_TEST xfail reason="operator continuation after case expression is not parsed" -}
module CaseDollarContinuation where

f x y =
  case x of
    Just z -> z
    Nothing -> id
    $ y
