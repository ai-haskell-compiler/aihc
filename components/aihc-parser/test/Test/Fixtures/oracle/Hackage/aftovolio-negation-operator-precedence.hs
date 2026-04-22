{- ORACLE_TEST xfail negation operator precedence mishandled causing roundtrip mismatch -}
module A where
f l = (-l - 1)
