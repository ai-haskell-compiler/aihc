{- ORACLE_TEST pass -}
module ModuleWarning {-# WARNING "This module is intentionally unstable" #-} where

warningValue :: Int
warningValue = 1
