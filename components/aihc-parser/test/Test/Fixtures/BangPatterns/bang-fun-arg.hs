{- ORACLE_TEST
id: bang-fun-arg
category: patterns
expected: pass
-}
{-# LANGUAGE BangPatterns #-}

module BangPatternsFunArg where

strictId :: Int -> Int
strictId !x = x
