{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}
module ComplicatedType where

sort :: (?cmp :: a -> a -> Bool) => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)
  where
    insert y [] = [y]
    insert y (z:zs) = if ?cmp y z then y : z : zs else z : insert y zs
