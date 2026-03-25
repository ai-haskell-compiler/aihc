{-# LANGUAGE UnboxedSums #-}
module Pattern where

f x = case x of
  (# | y #) -> y
  (# y | #) -> y
