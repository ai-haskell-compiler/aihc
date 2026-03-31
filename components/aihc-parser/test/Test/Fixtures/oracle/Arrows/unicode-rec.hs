{- ORACLE_TEST xfail arrow unicode rec -}
{-# LANGUAGE Arrows, UnicodeSyntax #-}
module ArrowUnicodeRec where

import Control.Arrow

counter a = proc reset → do
  rec output ← returnA ⤙ if reset then 0 else next
      next ← returnA ⤙ output + 1
  returnA ⤙ output
