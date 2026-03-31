{- ORACLE_TEST xfail arrow unicode conditional -}
{-# LANGUAGE Arrows, UnicodeSyntax #-}
module ArrowUnicodeConditional where

import Control.Arrow

f g h = proc (x, y) → do
  if True
    then g ⤙ x + 1
    else h ⤙ y + 2
