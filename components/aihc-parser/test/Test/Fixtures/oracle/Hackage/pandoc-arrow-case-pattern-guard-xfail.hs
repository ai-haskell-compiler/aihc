{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE PatternGuards #-}
module PandocArrowCasePatternGuard where

import Control.Arrow

f = proc x -> do
  case x of
    Right y | p y -> returnA -< y
    Left _ -> returnA -< 0
