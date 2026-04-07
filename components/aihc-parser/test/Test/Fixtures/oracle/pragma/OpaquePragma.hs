{- ORACLE_TEST pass -}
module OpaquePragma where

opaqueFun :: Int -> Int
opaqueFun x = x + 1
{-# OPAQUE opaqueFun #-}
