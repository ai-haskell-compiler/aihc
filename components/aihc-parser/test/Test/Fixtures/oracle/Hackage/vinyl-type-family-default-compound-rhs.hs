{- ORACLE_TEST xfail parser rejects type family default with compound right-hand side -}
{-# LANGUAGE TypeFamilies #-}
module VinylTypeFamilyDefaultCompoundRhs where

class C f where
  type T f = f Int
