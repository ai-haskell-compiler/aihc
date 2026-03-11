{-# OPTIONS -Wno-redundant-constraints #-}

module Data.Type.Witness.Specific.List.Sum where

import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessConstraint
import Data.Type.Witness.Specific.List.List
import Import
import Unsafe.Coerce

type ListSum :: [Type] -> Type
type family ListSum w = r | r -> w where
    ListSum '[] = Void
    ListSum (t ': tt) = Either t (ListSum tt)

-- | workaround for https://gitlab.haskell.org/ghc/ghc/issues/10833
injectiveListSum ::
       forall (a :: [Type]) (b :: [Type]). ListSum a ~ ListSum b
    => a :~: b
injectiveListSum = unsafeCoerce Refl

listSumEq :: (forall a. w a -> Dict (Eq a)) -> ListType w t -> Dict (Eq (ListSum t))
listSumEq _ NilListType = Dict
listSumEq f (ConsListType t tt) =
    case (f t, listSumEq f tt) of
        (Dict, Dict) -> Dict

listSumShow :: (forall a. w a -> Dict (Show a)) -> ListType w t -> Dict (Show (ListSum t))
listSumShow _ NilListType = Dict
listSumShow f (ConsListType t tt) =
    case (f t, listSumShow f tt) of
        (Dict, Dict) -> Dict

mapListSum :: ListType w t -> (forall a. w a -> a -> a) -> ListSum t -> ListSum t
mapListSum NilListType _f v = v
mapListSum (ConsListType wa _wr) f (Left a) = Left $ f wa a
mapListSum (ConsListType _wa wr) f (Right rest) = Right $ mapListSum wr f rest

type ListSumType :: (Type -> Type) -> (Type -> Type)
type role ListSumType representational nominal
data ListSumType wit t where
    MkListSumType :: forall (wit :: Type -> Type) (lt :: [Type]). ListType wit lt -> ListSumType wit (ListSum lt)

instance TestEquality wit => TestEquality (ListSumType wit) where
    testEquality (MkListSumType lt1) (MkListSumType lt2) =
        case testEquality lt1 lt2 of
            Just Refl -> Just Refl
            Nothing -> Nothing

instance WitnessConstraint Eq w => WitnessConstraint Eq (ListSumType w) where
    witnessConstraint (MkListSumType lt) = listSumEq witnessConstraint lt

instance Representative w => Representative (ListSumType w) where
    getRepWitness (MkListSumType NilListType) = Dict
    getRepWitness (MkListSumType (ConsListType a ar)) =
        case (getRepWitness a, getRepWitness $ MkListSumType ar) of
            (Dict, Dict) -> Dict

instance Representative w => Is (ListSumType w) Void where
    representative = MkListSumType NilListType

instance (Is w a, Is (ListSumType w) ar) => Is (ListSumType w) (Either a ar) where
    representative =
        case representative @_ @(ListSumType w) @ar of
            MkListSumType r -> MkListSumType $ ConsListType representative r
