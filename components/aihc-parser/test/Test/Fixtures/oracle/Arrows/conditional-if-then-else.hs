{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module ArrowConditionalIfThenElse where

import Control.Arrow

f g h = proc (x, y) -> do
  if True
    then g -< x + 1
    else h -< y + 2
