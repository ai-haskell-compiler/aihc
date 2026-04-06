{- ORACLE_TEST xfail RULES pragma not preserved in pretty-printer roundtrip -}
module RulesPragma where

{-# RULES
"map/id" forall xs. map id xs = xs
#-}

useMapId :: [Int] -> [Int]
useMapId xs = map id xs
