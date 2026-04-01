{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module DefaultTypeInstance where

class IsBoolMap v where
  type Key v
  type instance Key v = Int