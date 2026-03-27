{-# LANGUAGE TypeFamilies #-}
module Wildcards where

import Data.Kind (Type)

data family F a b :: Type
data instance F Int _ = FInt Int

type family T a :: Type
type instance T (a,_) = a

newtype instance F Bool a = FBoolA (a -> Int)
