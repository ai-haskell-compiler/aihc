{- ORACLE_TEST xfail arrow unicode fat arrow right -}
{-# LANGUAGE Arrows, UnicodeSyntax #-}
module ArrowUnicodeFatArrowRight where

import Control.Arrow

f g h = proc x → do
  y ← g ⤙ x
  h ⤙ y
