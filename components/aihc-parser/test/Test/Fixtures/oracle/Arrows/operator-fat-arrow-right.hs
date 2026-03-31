{- ORACLE_TEST xfail arrow operator fat arrow right -}
{-# LANGUAGE Arrows #-}
module ArrowOperatorFatArrowRight where

import Control.Arrow

f g h = proc x -> do
  y <- g -< x
  h -< y
