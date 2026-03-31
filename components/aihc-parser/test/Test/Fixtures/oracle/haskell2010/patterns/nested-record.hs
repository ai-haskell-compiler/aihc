{- ORACLE_TEST xfail as-pattern in constructor argument -}
module PatternNestedRecord where

data Pair a b = Pair a b

delete (Pair k@(x, y) z) = ()
