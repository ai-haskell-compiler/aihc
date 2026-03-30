{- ORACLE_TEST
id: hashable-xxhash-tests-numeric-underscores
category: corpus
expected: pass
reason: from hashable/tests/xxhash-tests.hs; parser now accepts NumericUnderscores literals
-}
{-# LANGUAGE NumericUnderscores #-}
module X where

x :: Integer
x = 0xc77b_3abb_6f87_acd9
