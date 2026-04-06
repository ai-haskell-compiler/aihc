{- ORACLE_TEST xfail INLINABLE pragma not preserved in pretty-printer roundtrip -}
module SpecializeImportedDef (lookupLike) where

lookupLike :: Eq a => [(a, b)] -> a -> Maybe b
lookupLike [] _ = Nothing
lookupLike ((k, v):xs) key
  | k == key = Just v
  | otherwise = lookupLike xs key
{-# INLINABLE lookupLike #-}
