{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module VinylTypeFamilyDefaultCompoundRhs where

class C f where
  type T f = f Int
