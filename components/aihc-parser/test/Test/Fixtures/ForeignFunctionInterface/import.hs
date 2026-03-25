{-# LANGUAGE ForeignFunctionInterface #-}
module Import where

foreign import ccall "f" f :: Int -> Int
