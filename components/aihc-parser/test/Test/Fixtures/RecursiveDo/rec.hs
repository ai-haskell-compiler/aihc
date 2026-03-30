{- ORACLE_TEST
id: rec
category: expressions
expected: xfail
reason: basic rec block
-}
{-# LANGUAGE RecursiveDo #-}
module Rec where

f = do
  rec
    x <- return 1
  return x
