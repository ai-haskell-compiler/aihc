{- ORACLE_TEST xfail basic default signature -}
{-# LANGUAGE DefaultSignatures #-}
module Basic where

class C a where
  f :: a -> Int
  default f :: (D a) => a -> Int
  f = g