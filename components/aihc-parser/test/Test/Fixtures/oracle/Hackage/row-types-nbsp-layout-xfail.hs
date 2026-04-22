{- ORACLE_TEST xfail non-breaking space (U+00A0) indentation breaks layout in where block -}
{-# LANGUAGE TypeFamilies #-}
module RowTypesNbspLayout where

type family F a where
  F a = a
  F b = b
