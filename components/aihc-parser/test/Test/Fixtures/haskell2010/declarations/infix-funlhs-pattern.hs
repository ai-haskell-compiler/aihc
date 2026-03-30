{- ORACLE_TEST pass -}
module InfixFunlhsPattern where
(Just a) <||> _ = Just a
Nothing <||> b = b