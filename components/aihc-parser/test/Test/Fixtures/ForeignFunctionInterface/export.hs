{-# LANGUAGE ForeignFunctionInterface #-}
module Export where

f :: Int -> Int
f x = x

foreign export ccall "f" f :: Int -> Int
