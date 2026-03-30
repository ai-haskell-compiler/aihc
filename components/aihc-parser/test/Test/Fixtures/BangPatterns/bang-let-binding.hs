{- ORACLE_TEST
id: bang-let-binding
category: patterns
expected: pass
-}
{-# LANGUAGE BangPatterns #-}

module BangPatternsLetBinding where

strictPair :: (Int, Int) -> Int
strictPair pair =
  let !(x, y) = pair
   in x + y
