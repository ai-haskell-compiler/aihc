{- ORACLE_TEST xfail pretty-printer parenthesizes spaced negative literal causing roundtrip mismatch -}
module A where
f x = x == - 1
