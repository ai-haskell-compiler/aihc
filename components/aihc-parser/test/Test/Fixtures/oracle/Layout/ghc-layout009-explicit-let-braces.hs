{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout009.hs.
module M where

f :: Char
f = let {x = 'a'} in x
