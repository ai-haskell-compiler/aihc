{-# LANGUAGE BinaryLiterals #-}

module BinaryLiteralsPatterns where

decodeBit :: Int -> String
decodeBit n = case n of
  0b0 -> "zero"
  0b1 -> "one"
  _ -> "many"
