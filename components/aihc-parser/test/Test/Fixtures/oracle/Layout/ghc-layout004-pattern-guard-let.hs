{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout004.hs.
{-# LANGUAGE PatternGuards #-}

module M where

f | Just x <- undefined,
    let y = x,
    undefined x y
  = ()
