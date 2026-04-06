{- ORACLE_TEST pass -}
module ModuleDeprecated {-# DEPRECATED "Use New.ModuleDeprecated instead" #-} where

deprecatedValue :: Int
deprecatedValue = 2
