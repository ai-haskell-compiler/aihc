{- ORACLE_TEST
id: bang-lambda
category: patterns
expected: pass
-}
{-# LANGUAGE BangPatterns #-}

module BangPatternsLambda where

applyStrict :: (Int -> Int) -> Int -> Int
applyStrict f = (\ !x -> f x)
