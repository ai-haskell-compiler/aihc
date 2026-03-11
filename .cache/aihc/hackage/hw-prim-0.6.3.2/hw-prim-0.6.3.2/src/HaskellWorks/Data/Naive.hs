module HaskellWorks.Data.Naive
    ( Naive(..)
    , naive
    ) where

newtype Naive a = Naive a deriving (Eq, Show)

naive :: Naive a -> a
naive (Naive a) = a
{-# INLINE naive #-}
