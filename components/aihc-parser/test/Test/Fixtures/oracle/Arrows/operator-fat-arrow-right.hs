{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module ArrowOperatorFatArrowRight where

import Control.Arrow

f g h = proc x -> do
  y <- g -< x
  h -< y
