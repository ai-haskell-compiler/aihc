{- ORACLE_TEST xfail command-level infix pretty-printing adds parens -}
{-# LANGUAGE Arrows #-}
module ArrowInfixInDo where

import Control.Arrow

expr' term = proc x -> do
  returnA -< x
  <+> do
    y <- term -< ()
    expr' term -< x + y
