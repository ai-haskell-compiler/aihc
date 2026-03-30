{- ORACLE_TEST pass -}
{-# LANGUAGE ForeignFunctionInterface #-}
module StdCall where

foreign import stdcall "f" f :: Int -> Int