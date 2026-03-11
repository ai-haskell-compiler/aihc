{-# OPTIONS -Wno-redundant-constraints #-}

module Data.Type.Witness.Specific.List.Product where

import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessConstraint
import Data.Type.Witness.Specific.List.Element
import Data.Type.Witness.Specific.List.List
import Data.Type.Witness.Specific.Some
import Import
import Unsafe.Coerce

type ListProduct :: [Type] -> Type
type family ListProduct w = r | r -> w where
    ListProduct '[] = ()
    ListProduct (t ': tt) = (t, ListProduct tt)

-- | workaround for https://gitlab.haskell.org/ghc/ghc/issues/10833
injectiveListProduct ::
       forall (a :: [Type]) (b :: [Type]). ListProduct a ~ ListProduct b
    => a :~: b
injectiveListProduct = unsafeCoerce Refl

listProductEq :: (forall a. w a -> Dict (Eq a)) -> ListType w t -> Dict (Eq (ListProduct t))
listProductEq _ NilListType = Dict
listProductEq f (ConsListType t tt) =
    case (f t, listProductEq f tt) of
        (Dict, Dict) -> Dict

listProductShow :: (forall a. w a -> Dict (Show a)) -> ListType w t -> Dict (Show (ListProduct t))
listProductShow _ NilListType = Dict
listProductShow f (ConsListType t tt) =
    case (f t, listProductShow f tt) of
        (Dict, Dict) -> Dict

fillListProduct :: ListType w t -> (forall a. w a -> a) -> ListProduct t
fillListProduct NilListType _f = ()
fillListProduct (ConsListType wa wr) f = (f wa, fillListProduct wr f)

mapListProduct :: ListType w t -> (forall a. w a -> a -> a) -> ListProduct t -> ListProduct t
mapListProduct NilListType _f () = ()
mapListProduct (ConsListType wa wr) f (a, rest) = (f wa a, mapListProduct wr f rest)

lift2ListProduct :: ListType w t -> (forall a. w a -> a -> a -> a) -> ListProduct t -> ListProduct t -> ListProduct t
lift2ListProduct NilListType _f () () = ()
lift2ListProduct (ConsListType wa wr) f (a, resta) (b, restb) = (f wa a b, lift2ListProduct wr f resta restb)

identityListProduct :: ListType Identity lt -> ListProduct lt
identityListProduct NilListType = ()
identityListProduct (ConsListType (Identity a) rest) = (a, identityListProduct rest)

sequenceListProduct :: Applicative f => ListType f lt -> f (ListProduct lt)
sequenceListProduct NilListType = pure ()
sequenceListProduct (ConsListType fa rest) = liftA2 (,) fa (sequenceListProduct rest)

listProductGetElement :: ListElementType list t -> ListProduct list -> t
listProductGetElement FirstElementType = fst -- using fst and snd for irrefutable matching
listProductGetElement (RestElementType lw) = listProductGetElement lw . snd

listProductPutElement :: ListElementType list t -> t -> ListProduct list -> ListProduct list
listProductPutElement FirstElementType t (_, r) = (t, r)
listProductPutElement (RestElementType lw) t (a, r) = (a, listProductPutElement lw t r)

listProductModifyElement :: ListElementType list t -> (t -> t) -> ListProduct list -> ListProduct list
listProductModifyElement n aa t = listProductPutElement n (aa (listProductGetElement n t)) t

listProductSequence ::
       forall f list. Applicative f
    => ListType f list
    -> f (ListProduct list)
listProductSequence NilListType = pure ()
listProductSequence (ConsListType t tt) = liftA2 (,) t $ listProductSequence tt

pickListProduct :: TestEquality w => ListType w lta -> w b -> Maybe (ListProduct lta -> b)
pickListProduct NilListType _ = Nothing
pickListProduct (ConsListType wa _) wb
    | Just Refl <- testEquality wa wb = Just fst
pickListProduct (ConsListType _ la) wb = fmap (\f -> f . snd) $ pickListProduct la wb

matchListProduct ::
       TestEquality w => ListType w lta -> ListType w ltb -> Either (Some w) (ListProduct lta -> ListProduct ltb)
matchListProduct _ NilListType = return $ \_ -> ()
matchListProduct la (ConsListType wb lb) =
    case pickListProduct la wb of
        Nothing -> Left $ MkSome wb
        Just f1 -> do
            fr <- matchListProduct la lb
            return $ \pa -> (f1 pa, fr pa)

type ListProductType :: (Type -> Type) -> (Type -> Type)
type role ListProductType representational nominal
data ListProductType wit t where
    MkListProductType
        :: forall (wit :: Type -> Type) (lt :: [Type]). ListType wit lt -> ListProductType wit (ListProduct lt)

instance TestEquality wit => TestEquality (ListProductType wit) where
    testEquality (MkListProductType lt1) (MkListProductType lt2) =
        case testEquality lt1 lt2 of
            Just Refl -> Just Refl
            Nothing -> Nothing

instance WitnessConstraint Eq w => WitnessConstraint Eq (ListProductType w) where
    witnessConstraint (MkListProductType lt) = listProductEq witnessConstraint lt

instance Representative w => Representative (ListProductType w) where
    getRepWitness (MkListProductType NilListType) = Dict
    getRepWitness (MkListProductType (ConsListType a ar)) =
        case (getRepWitness a, getRepWitness $ MkListProductType ar) of
            (Dict, Dict) -> Dict

instance Representative w => Is (ListProductType w) () where
    representative = MkListProductType NilListType

instance (Is w a, Is (ListProductType w) ar) => Is (ListProductType w) (a, ar) where
    representative =
        case representative @_ @(ListProductType w) @ar of
            MkListProductType r -> MkListProductType $ ConsListType representative r
