{- ORACLE_TEST pass -}
module CaseDollarContinuation where

f x y =
  case x of
    Just z -> z
    Nothing -> id
    $ y
