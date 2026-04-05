{- ORACLE_TEST pass -}
{-# LANGUAGE RecursiveDo #-}
module Rec where

f = do
  rec
    x <- return 1
  return x