{- ORACLE_TEST xfail expression-level SCC pragma annotation is dropped -}
module HappyMetaSccPragmaExpr where
f x = {-# SCC "label" #-} x
