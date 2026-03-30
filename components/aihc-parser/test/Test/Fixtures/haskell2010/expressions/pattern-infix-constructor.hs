{- ORACLE_TEST pass -}
module ExprS317PatInfixConstructor where
x xs = case xs of { y:ys -> y; [] -> 0 }