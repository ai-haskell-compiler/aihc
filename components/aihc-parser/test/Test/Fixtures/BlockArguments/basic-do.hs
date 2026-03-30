{- ORACLE_TEST
id: basic-do
category: expressions
expected: xfail
reason: basic do block argument
-}
{-# LANGUAGE BlockArguments #-}
module BasicDo where

f = id do
  pure ()
