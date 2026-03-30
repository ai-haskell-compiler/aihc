{- ORACLE_TEST
id: bang-where
category: patterns
expected: pass
-}
{-# LANGUAGE BangPatterns #-}

module BangPatternsWhere where

scale :: Int -> Int -> Int
scale factor input = go input
  where
    go !x = factor * x
