{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}
module SingletonType where

f :: (# Int #) -> (# Int #)
f (# x #) = (# x #)

g :: (# "hello" #) -> (# "hello" #)
g (# x #) = (# x #)

h :: (# 42 #) -> (# 42 #)
h (# x #) = (# x #)
