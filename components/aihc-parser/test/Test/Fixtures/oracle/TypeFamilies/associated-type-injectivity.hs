{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module AssociatedTypeInjectivity where

class C a where
  type Elem a = r | r -> a
