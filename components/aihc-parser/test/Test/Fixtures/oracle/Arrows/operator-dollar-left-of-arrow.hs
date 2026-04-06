{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module ArrowOperatorDollarLeftOfArrow where

import Control.Arrow

f g h = proc x -> do
  y <- g $ h -< x
  returnA -< y
