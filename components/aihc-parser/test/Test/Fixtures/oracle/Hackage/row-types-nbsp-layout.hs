{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module RowTypesNbspLayout where

type family F a where
  F a = a
  F b = b
