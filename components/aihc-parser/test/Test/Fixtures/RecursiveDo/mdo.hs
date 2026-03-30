{- ORACLE_TEST xfail basic mdo block -}
{-# LANGUAGE RecursiveDo #-}
module MDo where

f = mdo
  x <- return 1
  return x