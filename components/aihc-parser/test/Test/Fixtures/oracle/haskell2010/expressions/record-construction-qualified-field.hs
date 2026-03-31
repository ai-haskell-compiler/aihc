{- ORACLE_TEST pass -}
module RecordConstructionQualifiedField where

data R = R {field :: Int}

x = R {Qual.field = 10}
