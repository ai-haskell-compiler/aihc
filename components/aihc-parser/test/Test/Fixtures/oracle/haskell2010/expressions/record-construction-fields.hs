{- ORACLE_TEST pass -}
module ExprS315RecordConstructionFields where
data R = R { a :: Int, b :: Int }
x = R { a = 1, b = 2 }