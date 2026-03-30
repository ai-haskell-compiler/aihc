{- ORACLE_TEST xfail basic rec block -}
{-# LANGUAGE RecursiveDo #-}
module Rec where

f = do
  rec
    x <- return 1
  return x