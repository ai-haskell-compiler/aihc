{- ORACLE_TEST pass -}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}
{-# INCLUDE "compat.h" #-}

module FileHeaderPragmas where

foreign import ccall unsafe "math.h sin" c_sin :: Double -> Double

answer :: Int
answer = 42
