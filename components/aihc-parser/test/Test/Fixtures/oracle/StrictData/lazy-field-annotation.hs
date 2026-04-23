{- ORACLE_TEST pass -}
module LazyFieldAnnotation where

data MapF k v r = TipF | BinF Int k ~v r r
