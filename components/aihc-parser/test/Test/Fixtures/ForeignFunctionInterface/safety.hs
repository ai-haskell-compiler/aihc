{- ORACLE_TEST
id: safety
category: declarations
expected: pass
-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Safety where

foreign import ccall safe "f" f_safe :: Int -> Int
foreign import ccall unsafe "g" f_unsafe :: Int -> Int
