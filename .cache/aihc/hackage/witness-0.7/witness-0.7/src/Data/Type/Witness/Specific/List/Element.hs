module Data.Type.Witness.Specific.List.Element where

import Data.PeanoNat
import Data.Type.Witness.General.Order
import Data.Type.Witness.Specific.List.List
import Data.Type.Witness.Specific.PeanoNat
import Data.Type.Witness.Specific.Some
import Import

type ListElementType :: forall k. [k] -> k -> Type
type role ListElementType nominal nominal
data ListElementType kk t where
    FirstElementType :: ListElementType (t ': tt) t
    RestElementType :: ListElementType aa t -> ListElementType (a ': aa) t

instance TestEquality (ListElementType tt) where
    testEquality FirstElementType FirstElementType = Just Refl
    testEquality (RestElementType lt1) (RestElementType lt2) = do
        Refl <- testEquality lt1 lt2
        return Refl
    testEquality _ _ = Nothing

instance TestOrder (ListElementType tt) where
    testCompare FirstElementType FirstElementType = WEQ
    testCompare (RestElementType a) (RestElementType b) =
        case testCompare a b of
            WLT -> WLT
            WGT -> WGT
            WEQ -> WEQ
    testCompare (RestElementType _) FirstElementType = WGT
    testCompare FirstElementType (RestElementType _) = WLT

instance Searchable (ListElementType '[] t) where
    search = finiteSearch

instance Eq (ListElementType tt t) where
    lt1 == lt2 = isJust $ testEquality lt1 lt2

instance Countable (ListElementType '[] t) where
    countPrevious = finiteCountPrevious
    countMaybeNext = finiteCountMaybeNext

instance Finite (ListElementType '[] t) where
    allValues = []

instance Empty (ListElementType '[] t) where
    never lt = case lt of {}

pickListElement :: forall k (w :: k -> Type) (t :: k) (lt :: [k]). ListElementType lt t -> ListType w lt -> w t
pickListElement FirstElementType (ConsListType wt _) = wt
pickListElement (RestElementType n) (ConsListType _ l) = pickListElement n l

lookUpListElement ::
       forall k (w :: k -> Type) (t :: k) (lt :: [k]). TestEquality w
    => w t
    -> ListType w lt
    -> Maybe (ListElementType lt t)
lookUpListElement _ NilListType = Nothing
lookUpListElement wt (ConsListType wt' _)
    | Just Refl <- testEquality wt wt' = Just FirstElementType
lookUpListElement wt (ConsListType _ lt) = do
    et <- lookUpListElement wt lt
    return $ RestElementType et

countListType :: ListType w lt -> ListType (ListElementType lt) lt
countListType NilListType = NilListType
countListType (ConsListType _ lt) = ConsListType FirstElementType (mapListType RestElementType $ countListType lt)

listElementTypeIndex :: Some (ListElementType lt) -> Some (Greater (ListLength lt))
listElementTypeIndex (MkSome FirstElementType) = MkSome $ MkGreater ZeroGreaterEqual
listElementTypeIndex (MkSome (RestElementType n)) =
    case listElementTypeIndex $ MkSome n of
        MkSome (MkGreater n') -> MkSome $ MkGreater $ SuccGreaterEqual n'

indexListElementType :: ListType w lt -> Some (Greater (ListLength lt)) -> SomeFor w (ListElementType lt)
indexListElementType (ConsListType wa _) (MkSome (MkGreater ZeroGreaterEqual)) = MkSomeFor FirstElementType wa
indexListElementType (ConsListType _ lt) (MkSome (MkGreater (SuccGreaterEqual n))) =
    case indexListElementType lt (MkSome (MkGreater n)) of
        MkSomeFor n' wa -> MkSomeFor (RestElementType n') wa
indexListElementType NilListType (MkSome n) = never n
