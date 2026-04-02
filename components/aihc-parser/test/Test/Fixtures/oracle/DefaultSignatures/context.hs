{- ORACLE_TEST pass -}
{-# LANGUAGE DefaultSignatures #-}
module Context where

class C a where
  f :: a -> Int
  default f :: D a => a -> Int
  f = g
