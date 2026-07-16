{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout001.hs.
module M where

f = case () of
  () -> ()
  where x = x
