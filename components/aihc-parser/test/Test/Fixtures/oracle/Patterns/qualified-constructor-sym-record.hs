{- ORACLE_TEST pass -}
-- Regression test: A.:+ is the :+ constructor from module A, not a single
-- constructor named "A.:+". Verify that qualified symbolic constructors
-- parse correctly in record patterns.
module QualifiedConstructorSymRecord where

f x = x
  where
    g (A.:+) {} = ()
