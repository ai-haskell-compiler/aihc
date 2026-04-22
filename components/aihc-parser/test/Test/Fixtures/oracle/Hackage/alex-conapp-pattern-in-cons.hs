{- ORACLE_TEST xfail pretty-printer wraps constructor-application pattern in cons with extra parens causing roundtrip mismatch -}
module A where
data T = C Int
f (C x : xs) = x
