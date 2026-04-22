{- ORACLE_TEST pass -}
module HappyMetaSccPragmaExpr where
f x = {-# SCC "label" #-} x
