{- ORACLE_TEST
id: basic
category: declarations
expected: xfail
reason: basic default signature
-}
{-# LANGUAGE DefaultSignatures #-}
module Basic where

class C a where
  f :: a -> Int
  default f :: (D a) => a -> Int
  f = g
