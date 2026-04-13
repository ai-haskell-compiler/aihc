{- ORACLE_TEST pass -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module PrefixPatternQualifiers where

comp xs = [y | K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs]

guardK xs
  | K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs = y
guardK _ = 0

doK xs = do
  K !y ~(Just z) q@(Right _) ((negate -> n)) (-1) <- xs
  pure y
