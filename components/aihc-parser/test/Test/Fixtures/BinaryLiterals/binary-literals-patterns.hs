{- ORACLE_TEST
id: binary-literals-patterns
category: patterns
expected: pass
-}
{-# LANGUAGE BinaryLiterals #-}

module BinaryLiteralsPatterns where

decodeBit :: Int -> String
decodeBit n = case n of
  0b0 -> "zero"
  0b1 -> "one"
  _ -> "many"
