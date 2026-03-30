{- ORACLE_TEST
id: context
category: declarations
expected: xfail
reason: default signature with context
-}
{-# LANGUAGE DefaultSignatures #-}
module Context where

class C a where
  f :: a -> Int
  default f :: D a => a -> Int
  f = g
