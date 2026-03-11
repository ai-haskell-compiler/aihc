module Data.Eq.HT where

import Data.Function.HT (compose2, )

{-# INLINE equating #-}
equating :: Eq b => (a -> b) -> a -> a -> Bool
equating = compose2 (==)
