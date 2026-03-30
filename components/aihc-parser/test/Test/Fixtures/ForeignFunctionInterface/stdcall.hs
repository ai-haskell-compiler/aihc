{- ORACLE_TEST
id: stdcall
category: declarations
expected: pass
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module StdCall where

foreign import stdcall "f" f :: Int -> Int
