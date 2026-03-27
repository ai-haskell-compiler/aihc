{-# LANGUAGE TypeFamilies #-}
module AssociatedType where

class Collects ce where
  type Elem ce :: Type
