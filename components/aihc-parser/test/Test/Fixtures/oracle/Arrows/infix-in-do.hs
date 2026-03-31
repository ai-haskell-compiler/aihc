{- ORACLE_TEST xfail arrow infix operator in do -}
{-# LANGUAGE Arrows #-}
module ArrowInfixInDo where

import Control.Arrow

expr' term = proc x -> do
  returnA -< x
  <+> do
    y <- term -< ()
    expr' term -< x + y
