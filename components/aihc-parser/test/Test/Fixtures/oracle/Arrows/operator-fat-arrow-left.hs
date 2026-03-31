{- ORACLE_TEST xfail arrow operator fat arrow left -}
{-# LANGUAGE Arrows #-}
module ArrowOperatorFatArrowLeft where

import Control.Arrow

f g = proc x -> g -< x
