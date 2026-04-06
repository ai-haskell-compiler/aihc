{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module ArrowOperatorFatArrowLeft where

import Control.Arrow

f g = proc x -> g -< x
