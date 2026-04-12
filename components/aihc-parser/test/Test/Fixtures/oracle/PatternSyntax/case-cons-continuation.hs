{- ORACLE_TEST xfail cons operator at start of line after case expression not parsed -}
{-# LANGUAGE LambdaCase #-}

module CaseConsContinuation where

data E = A | B | C

f :: E -> String
f x =
  case x of
    A -> "a"
    B -> "b"
    C -> "c"
  : "tail"
