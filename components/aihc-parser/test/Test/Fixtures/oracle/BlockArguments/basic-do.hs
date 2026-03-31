{- ORACLE_TEST xfail parser accepts but pretty-printer roundtrip formatting differs -}
{-# LANGUAGE BlockArguments #-}
module BasicDo where

f = id do
  pure ()