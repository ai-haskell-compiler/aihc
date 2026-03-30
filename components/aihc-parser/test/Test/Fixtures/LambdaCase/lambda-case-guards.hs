{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseGuards where

parity :: Int -> String
parity = \case
  n | even n -> "even"
  _ -> "odd"