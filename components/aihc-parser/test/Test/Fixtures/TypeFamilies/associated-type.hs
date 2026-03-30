{- ORACLE_TEST xfail associated type family -}
{-# LANGUAGE TypeFamilies #-}
module AssociatedType where

import Data.Kind (Type)

class Collects ce where
  type Elem ce :: Type