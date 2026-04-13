{- ORACLE_TEST pass -}
{-# LANGUAGE BangPatterns #-}
module BangPatternsLetGuarded where

test :: Bool -> Int
test positive =
  let !x | True = 1
  in x
