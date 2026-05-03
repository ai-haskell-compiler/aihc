{- ORACLE_TEST pass -}
module DoLetInExpressionStmt where

f m = do
  let x = 1 in m x
