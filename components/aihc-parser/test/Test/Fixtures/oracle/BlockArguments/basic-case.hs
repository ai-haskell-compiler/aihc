{- ORACLE_TEST xfail parser accepts but pretty-printer roundtrip formatting differs -}
{-# LANGUAGE BlockArguments #-}
module BasicCase where

f x = id case x of
  _ -> ()