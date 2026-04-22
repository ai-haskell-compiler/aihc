{- ORACLE_TEST xfail negation prefix precedence: pretty-printer emits (- 1) / 2 instead of - 1 / 2 -}
module A where
f = - 1 / 2
