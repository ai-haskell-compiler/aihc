{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module ProcBlockLetArrowLhs where

f g = proc x -> (g let { y = x } in y) -< ()
