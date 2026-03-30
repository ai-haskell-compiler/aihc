{- ORACLE_TEST
id: lambda-case-guards
category: expressions
expected: pass
-}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseGuards where

parity :: Int -> String
parity = \case
  n | even n -> "even"
  _ -> "odd"
