{- ORACLE_TEST xfail pretty-printer wraps banged constructor pattern in extra parens causing roundtrip mismatch -}
{-# LANGUAGE BangPatterns #-}
module A where
f x = case x of
  !EQ -> True
  _ -> False
