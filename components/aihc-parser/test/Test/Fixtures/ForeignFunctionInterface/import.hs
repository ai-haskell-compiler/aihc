{- ORACLE_TEST
id: import
category: declarations
expected: pass
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Import where

foreign import ccall "f" f :: Int -> Int
