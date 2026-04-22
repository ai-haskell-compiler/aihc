{- ORACLE_TEST pass -}
{-# LANGUAGE BangPatterns #-}
module A where
f x = case x of
  !EQ -> True
  _ -> False
