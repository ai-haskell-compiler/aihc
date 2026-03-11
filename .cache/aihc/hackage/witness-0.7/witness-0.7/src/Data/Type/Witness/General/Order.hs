module Data.Type.Witness.General.Order where

import Import

type role WOrdering nominal nominal
data WOrdering (a :: k) (b :: k) where
    WLT :: forall k (a :: k) (b :: k). WOrdering a b
    WEQ :: forall k (a :: k). WOrdering a a
    WGT :: forall k (a :: k) (b :: k). WOrdering a b

wOrderingToOrdering :: WOrdering a b -> Ordering
wOrderingToOrdering WLT = LT
wOrderingToOrdering WEQ = EQ
wOrderingToOrdering WGT = GT

class TestEquality w => TestOrder (w :: k -> Type) where
    testCompare :: forall (a :: k) (b :: k). w a -> w b -> WOrdering a b
