module Data.Type.Witness.Specific.Natural
    ( KnownNat
    , type (<=)
    , type (+)
    , type (*)
    , type (^)
    , type (-)
    , CmpNat
    , Div
    , Mod
    , Log2
    , Nat
    , NaturalType(..)
    , zeroNaturalType
    , succNaturalType
    , addNaturalType
    , multiplyNaturalType
    , PeanoToNatural
    , peanoToNaturalType
    ) where

import Data.PeanoNat
import Data.Type.Witness.General.AllConstraint
import Data.Type.Witness.General.Order
import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessValue
import Data.Type.Witness.Specific.PeanoNat
import GHC.TypeNats
import Import
import Unsafe.Coerce

type NaturalType :: Nat -> Type
type role NaturalType nominal
data NaturalType bn where
    MkNaturalType :: KnownNat bn => NaturalType bn

instance WitnessValue NaturalType where
    type WitnessValueType NaturalType = Natural
    witnessToValue :: forall t. NaturalType t -> Natural
    witnessToValue MkNaturalType = natVal (Proxy :: Proxy t)
    valueToWitness i cont =
        case someNatVal i of
            SomeNat p -> let
                psw :: forall (t :: Nat). KnownNat t
                    => Proxy t
                    -> NaturalType t
                psw _ = MkNaturalType
                in cont $ psw p

instance TestEquality NaturalType where
    testEquality (MkNaturalType :: NaturalType a) (MkNaturalType :: NaturalType b) = sameNat (Proxy @a) (Proxy @b)

instance TestOrder NaturalType where
    testCompare a b =
        case testEquality a b of
            Just Refl -> WEQ
            Nothing ->
                if witnessToValue a > witnessToValue b
                    then WGT
                    else WLT

instance Representative NaturalType where
    getRepWitness MkNaturalType = Dict

instance KnownNat bn => Is NaturalType bn where
    representative = MkNaturalType

instance Show (NaturalType bn) where
    show = show . witnessToValue

instance AllConstraint Show NaturalType where
    allConstraint = Dict

unsafeNaturalType :: forall n. Natural -> NaturalType n
unsafeNaturalType i = valueToWitness i $ \(nt :: NaturalType t) -> unsafeCoerce @(NaturalType t) @(NaturalType n) nt

zeroNaturalType :: NaturalType 0
zeroNaturalType = MkNaturalType

succNaturalType :: NaturalType a -> NaturalType (a + 1)
succNaturalType a = unsafeNaturalType $ succ $ witnessToValue a

addNaturalType :: NaturalType a -> NaturalType b -> NaturalType (a + b)
addNaturalType a b = unsafeNaturalType $ witnessToValue a + witnessToValue b

multiplyNaturalType :: NaturalType a -> NaturalType b -> NaturalType (a * b)
multiplyNaturalType a b = unsafeNaturalType $ witnessToValue a * witnessToValue b

peanoToNaturalType :: PeanoNatType pn -> NaturalType (PeanoToNatural pn)
peanoToNaturalType ZeroType = zeroNaturalType
peanoToNaturalType (SuccType n) = succNaturalType $ peanoToNaturalType n
