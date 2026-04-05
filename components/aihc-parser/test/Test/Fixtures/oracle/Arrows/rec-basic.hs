{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module ArrowRecBasic where

import Control.Arrow

counter a = proc reset -> do
  rec output <- returnA -< if reset then 0 else next
      next <- returnA -< output + 1
  returnA -< output
