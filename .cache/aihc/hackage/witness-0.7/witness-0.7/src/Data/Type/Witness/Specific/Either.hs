module Data.Type.Witness.Specific.Either where

import Data.Type.Witness.General.AllConstraint
import Data.Type.Witness.General.Finite
import Data.Type.Witness.General.ListElement
import Data.Type.Witness.General.WitnessConstraint
import Data.Type.Witness.Specific.All
import Data.Type.Witness.Specific.List.Element
import Data.Type.Witness.Specific.Single
import Import

type EitherType :: forall k. (k -> Type) -> (k -> Type) -> (k -> Type)
type role EitherType representational representational nominal
data EitherType w1 w2 t
    = LeftType (w1 t)
    | RightType (w2 t)

instance (TestEquality w1, TestEquality w2) => TestEquality (EitherType w1 w2) where
    testEquality (LeftType s1) (LeftType s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality (RightType s1) (RightType s2) = do
        Refl <- testEquality s1 s2
        return Refl
    testEquality _ _ = Nothing

instance (FiniteWitness p, FiniteWitness q) => FiniteWitness (EitherType p q) where
    assembleAllFor getsel =
        (\(MkAllFor p) (MkAllFor q) ->
             MkAllFor $ \wt ->
                 case wt of
                     LeftType rt -> p rt
                     RightType rt -> q rt) <$>
        assembleAllFor (getsel . LeftType) <*>
        assembleAllFor (getsel . RightType)

instance (WitnessConstraint c p, WitnessConstraint c q) => WitnessConstraint c (EitherType p q) where
    witnessConstraint (LeftType rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict
    witnessConstraint (RightType rt) =
        case witnessConstraint @_ @c rt of
            Dict -> Dict

instance (Show (p t), Show (q t)) => Show (EitherType p q t) where
    show (LeftType rt) = show rt
    show (RightType rt) = show rt

instance (AllConstraint Show p, AllConstraint Show q) => AllConstraint Show (EitherType p q) where
    allConstraint :: forall t. Dict (Show (EitherType p q t))
    allConstraint =
        case allConstraint @_ @_ @Show @p @t of
            Dict ->
                case allConstraint @_ @_ @Show @q @t of
                    Dict -> Dict

eitherAllOf :: AllOf sel1 -> AllOf sel2 -> AllOf (EitherType sel1 sel2)
eitherAllOf (MkAllOf tup1) (MkAllOf tup2) =
    MkAllOf $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel

eitherAllFor :: AllFor f sel1 -> AllFor f sel2 -> AllFor f (EitherType sel1 sel2)
eitherAllFor (MkAllFor tup1) (MkAllFor tup2) =
    MkAllFor $ \esel ->
        case esel of
            LeftType sel -> tup1 sel
            RightType sel -> tup2 sel

type ConsType :: forall k. k -> (k -> Type) -> k -> Type
type ConsType a = EitherType (SingleType a)

instance ListElementWitness lt => ListElementWitness (ConsType a lt) where
    type WitnessTypeList (ConsType a lt) = a ': (WitnessTypeList lt)
    toListElementWitness (LeftType Refl) = FirstElementType
    toListElementWitness (RightType sel) = RestElementType $ toListElementWitness sel
    fromListElementWitness FirstElementType = LeftType Refl
    fromListElementWitness (RestElementType lt) = RightType $ fromListElementWitness lt
