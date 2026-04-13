{- ORACLE_TEST pass -}
{-# LANGUAGE StrictData #-}

module LazyFieldAnnotation where

data MapF k v r = TipF | BinF Int k ~v r r
