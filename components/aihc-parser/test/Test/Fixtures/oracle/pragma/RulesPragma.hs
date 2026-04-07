{- ORACLE_TEST pass -}
module RulesPragma where

{-# RULES
"map/id" forall xs. map id xs = xs
#-}

useMapId :: [Int] -> [Int]
useMapId xs = map id xs
