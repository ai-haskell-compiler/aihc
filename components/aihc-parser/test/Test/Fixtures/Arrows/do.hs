{- ORACLE_TEST
id: do
category: expressions
expected: xfail
reason: arrow notation with do
-}
{-# LANGUAGE Arrows #-}
module Do where

f g h = proc x -> do
  y <- g -< x
  h -< y
