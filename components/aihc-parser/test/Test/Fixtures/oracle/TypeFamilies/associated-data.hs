{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
module AssociatedData where

import Data.Kind (Type)

class GMapKey k where
  data GMap k :: Type -> Type