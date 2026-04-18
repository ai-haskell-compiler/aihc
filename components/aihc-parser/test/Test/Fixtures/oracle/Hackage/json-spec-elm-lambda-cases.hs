{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}

module M where

f = \cases
  (True) (False) -> 0
  (_) (_) -> 1
