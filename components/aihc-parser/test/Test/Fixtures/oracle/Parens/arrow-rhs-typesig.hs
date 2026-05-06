{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module M where

import Control.Arrow

f = proc string -> do
  count <- sumC -< 1 :: Integer
  returnA -< count
