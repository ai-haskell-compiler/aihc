{- ORACLE_TEST
id: associated-data
category: declarations
expected: xfail
reason: associated data family
-}
{-# LANGUAGE TypeFamilies #-}
module AssociatedData where

import Data.Kind (Type)

class GMapKey k where
  data GMap k :: Type -> Type
