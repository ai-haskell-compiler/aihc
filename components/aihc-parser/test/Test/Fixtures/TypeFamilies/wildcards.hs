{-# LANGUAGE TypeFamilies #-}
module Wildcards where

data family F a b :: *
data instance F Int _ = Int

type family T a :: *
type instance T (a,_) = a

data instance F Bool _a = _a -> Int
