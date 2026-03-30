{- ORACLE_TEST
id: binary-literals-negative
category: literals
expected: pass
reason: parser now supports negative binary literals
-}
{-# LANGUAGE BinaryLiterals #-}

module BinaryLiteralsNegative where

signedValues :: [Int]
signedValues = [0b1, -0b1, -0B10, 0b11]
