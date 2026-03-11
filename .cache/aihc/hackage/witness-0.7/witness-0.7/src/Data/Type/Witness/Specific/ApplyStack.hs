module Data.Type.Witness.Specific.ApplyStack where

import Data.Type.Witness.General.Representative
import Data.Type.Witness.Specific.Concat
import Data.Type.Witness.Specific.List.List
import Import

type ApplyStack :: forall k. [k -> k] -> k -> k
type family ApplyStack f a where
    ApplyStack '[] a = a
    ApplyStack (t ': tt) a = t (ApplyStack tt a)

witApplyConcatRefl ::
       forall k (f1 :: [k -> k]) (f2 :: [k -> k]) (a :: k) w.
       ListType w f1
    -> (ApplyStack (Concat f1 f2) a) :~: (ApplyStack f1 (ApplyStack f2 a))
witApplyConcatRefl NilListType = Refl
witApplyConcatRefl (ConsListType _ lt) =
    case witApplyConcatRefl @k @_ @f2 @a lt of
        Refl -> Refl

applyConcatRefl ::
       forall k (f1 :: [k -> k]) (f2 :: [k -> k]) (a :: k) w. Is (ListType w) f1
    => (ApplyStack (Concat f1 f2) a) :~: (ApplyStack f1 (ApplyStack f2 a))
applyConcatRefl = witApplyConcatRefl @k @f1 @f2 @a $ representative @_ @(ListType w) @f1
