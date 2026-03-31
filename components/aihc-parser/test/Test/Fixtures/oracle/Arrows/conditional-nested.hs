{- ORACLE_TEST xfail arrow conditional nested if -}
{-# LANGUAGE Arrows #-}
module ArrowConditionalNested where

import Control.Arrow

f g h k = proc (x, y) -> do
  if True
    then if False
           then g -< x
           else h -< y
    else k -< x + y
