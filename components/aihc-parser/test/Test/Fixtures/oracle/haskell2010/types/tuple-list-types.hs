{- ORACLE_TEST pass -}
module T2 where
f :: [Int] -> (Int, Int)
f xs = (length xs, sum xs)