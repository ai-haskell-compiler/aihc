{- ORACLE_TEST
id: mdo
category: expressions
expected: xfail
reason: basic mdo block
-}
{-# LANGUAGE RecursiveDo #-}
module MDo where

f = mdo
  x <- return 1
  return x
