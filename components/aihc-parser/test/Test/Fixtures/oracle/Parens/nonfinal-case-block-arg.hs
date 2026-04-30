{- ORACLE_TEST pass -}
{-# LANGUAGE BlockArguments #-}
module M where

x = finally
  case a of
    True -> b
    False -> c
  do
    d
