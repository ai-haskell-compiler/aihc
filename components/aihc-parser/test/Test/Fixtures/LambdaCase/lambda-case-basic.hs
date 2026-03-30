{- ORACLE_TEST
id: lambda-case-basic
category: expressions
expected: pass
-}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseBasic where

describeBool :: Bool -> String
describeBool = \case
  True -> "yes"
  False -> "no"

