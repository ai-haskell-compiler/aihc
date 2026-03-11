module Data.Type.Witness.Specific.Symbol
    ( module Data.Type.Witness.Specific.Symbol
    , Symbol
    , KnownSymbol
    ) where

import Data.Type.Witness.General.AllConstraint
import Data.Type.Witness.General.Order
import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessValue
import GHC.TypeLits
import Import

type SymbolType :: Symbol -> Type
type role SymbolType nominal
data SymbolType symbol where
    MkSymbolType :: KnownSymbol symbol => SymbolType symbol

instance WitnessValue SymbolType where
    type WitnessValueType SymbolType = String
    witnessToValue :: forall t. SymbolType t -> String
    witnessToValue MkSymbolType = symbolVal (Proxy :: Proxy t)
    valueToWitness s cont =
        case someSymbolVal s of
            SomeSymbol p -> let
                psw :: forall (t :: Symbol). KnownSymbol t
                    => Proxy t
                    -> SymbolType t
                psw _ = MkSymbolType
                in cont $ psw p

instance TestEquality SymbolType where
    testEquality (MkSymbolType :: SymbolType a) (MkSymbolType :: SymbolType b) = sameSymbol (Proxy @a) (Proxy @b)

instance TestOrder SymbolType where
    testCompare a b =
        case testEquality a b of
            Just Refl -> WEQ
            Nothing ->
                if witnessToValue a > witnessToValue b
                    then WGT
                    else WLT

instance Representative SymbolType where
    getRepWitness MkSymbolType = Dict

instance KnownSymbol symbol => Is SymbolType symbol where
    representative = MkSymbolType

instance Show (SymbolType symbol) where
    show = witnessToValue

instance AllConstraint Show SymbolType where
    allConstraint = Dict
