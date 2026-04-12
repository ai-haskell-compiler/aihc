{- ORACLE_TEST xfail reason="lazy field annotation in data constructor not parsed" -}
{-# LANGUAGE StrictData #-}

module LazyFieldAnnotation where

data MapF k v r = TipF | BinF Int k ~v r r
