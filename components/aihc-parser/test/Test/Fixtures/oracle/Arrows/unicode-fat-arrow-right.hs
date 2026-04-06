{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows, UnicodeSyntax #-}
module ArrowUnicodeFatArrowRight where

import Control.Arrow

f g h = proc x → do
  y ← g ⤙ x
  h ⤙ y
