{- ORACLE_TEST
id: export
category: declarations
expected: pass
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Export where

f :: Int -> Int
f x = x

foreign export ccall "f" f :: Int -> Int
