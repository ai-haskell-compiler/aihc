{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout002.hs.
module M where

f = if True then do undefined else undefined
