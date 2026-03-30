{- ORACLE_TEST xfail basic case block argument -}
{-# LANGUAGE BlockArguments #-}
module BasicCase where

f x = id case x of
  _ -> ()