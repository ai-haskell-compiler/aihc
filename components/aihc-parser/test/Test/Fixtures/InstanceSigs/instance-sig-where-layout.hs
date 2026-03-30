{- ORACLE_TEST
id: instance-sig-where-layout
category: declarations
expected: pass
-}
{-# LANGUAGE InstanceSigs #-}

module InstanceSigsWhereLayout where

class Measure a where
  measure :: a -> Int

instance Measure [a] where
  measure :: [a] -> Int
  measure xs =
    let go ys = length ys
     in go xs
