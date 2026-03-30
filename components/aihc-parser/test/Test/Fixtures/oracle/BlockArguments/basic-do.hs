{- ORACLE_TEST xfail basic do block argument -}
{-# LANGUAGE BlockArguments #-}
module BasicDo where

f = id do
  pure ()