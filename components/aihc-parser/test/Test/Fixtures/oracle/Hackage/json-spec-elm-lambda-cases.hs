{- ORACLE_TEST xfail reason="multi-argument \\cases alternatives are rejected after the first branch" -}
{-# LANGUAGE LambdaCase #-}

module M where

f = \cases
  True False -> 0
  _ _ -> 1
