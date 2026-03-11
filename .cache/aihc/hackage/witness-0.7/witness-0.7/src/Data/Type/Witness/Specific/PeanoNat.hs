module Data.Type.Witness.Specific.PeanoNat where

import Data.PeanoNat
import Data.Type.Witness.General.Order
import Data.Type.Witness.General.Representative
import Data.Type.Witness.General.WitnessValue
import Import

type PeanoNatType :: PeanoNat -> Type
type role PeanoNatType nominal
data PeanoNatType t where
    ZeroType :: PeanoNatType 'Zero
    SuccType :: PeanoNatType t -> PeanoNatType ('Succ t)

instance TestEquality PeanoNatType where
    testEquality ZeroType ZeroType = return Refl
    testEquality (SuccType a) (SuccType b) = do
        Refl <- testEquality a b
        return Refl
    testEquality _ _ = Nothing

instance TestOrder PeanoNatType where
    testCompare ZeroType ZeroType = WEQ
    testCompare (SuccType a) (SuccType b) =
        case testCompare a b of
            WLT -> WLT
            WGT -> WGT
            WEQ -> WEQ
    testCompare (SuccType _) ZeroType = WGT
    testCompare ZeroType (SuccType _) = WLT

instance Representative PeanoNatType where
    getRepWitness ZeroType = Dict
    getRepWitness (SuccType n) =
        case getRepWitness n of
            Dict -> Dict

instance Is PeanoNatType 'Zero where
    representative = ZeroType

instance Is PeanoNatType n => Is PeanoNatType ('Succ n) where
    representative = SuccType representative

instance WitnessValue PeanoNatType where
    type WitnessValueType PeanoNatType = PeanoNat
    witnessToValue :: forall t. PeanoNatType t -> PeanoNat
    witnessToValue ZeroType = Zero
    witnessToValue (SuccType n) = Succ $ witnessToValue n
    valueToWitness Zero cont = cont ZeroType
    valueToWitness (Succ n) cont = valueToWitness n $ \t -> cont $ SuccType t

type GreaterEqual :: PeanoNat -> PeanoNat -> Type
type role GreaterEqual nominal nominal
data GreaterEqual a b where
    ZeroGreaterEqual :: GreaterEqual a 'Zero
    SuccGreaterEqual :: GreaterEqual a b -> GreaterEqual ('Succ a) ('Succ b)

greaterEqualIndex :: GreaterEqual a b -> PeanoNatType b
greaterEqualIndex ZeroGreaterEqual = ZeroType
greaterEqualIndex (SuccGreaterEqual n) = SuccType $ greaterEqualIndex n

samePeanoGreaterEqual :: PeanoNatType a -> GreaterEqual a a
samePeanoGreaterEqual ZeroType = ZeroGreaterEqual
samePeanoGreaterEqual (SuccType a) = SuccGreaterEqual $ samePeanoGreaterEqual a

diff1GreaterEqual :: GreaterEqual a b -> GreaterEqual ('Succ a) b
diff1GreaterEqual ZeroGreaterEqual = ZeroGreaterEqual
diff1GreaterEqual (SuccGreaterEqual ge) = SuccGreaterEqual $ diff1GreaterEqual ge

peanoGreaterEqual :: PeanoNatType a -> PeanoNatType b -> Maybe (GreaterEqual a b)
peanoGreaterEqual _ ZeroType = return ZeroGreaterEqual
peanoGreaterEqual ZeroType _ = Nothing
peanoGreaterEqual (SuccType a) (SuccType b) = do
    al <- peanoGreaterEqual a b
    return $ SuccGreaterEqual al

addPeanoNatType :: PeanoNatType a -> PeanoNatType b -> PeanoNatType (Add a b)
addPeanoNatType ZeroType b = b
addPeanoNatType (SuccType a) b = SuccType $ addPeanoNatType a b

addZeroPeanoNatTypeEqual :: PeanoNatType a -> Add a 'Zero :~: a
addZeroPeanoNatTypeEqual ZeroType = Refl
addZeroPeanoNatTypeEqual (SuccType a) =
    case addZeroPeanoNatTypeEqual a of
        Refl -> Refl

succAddPeanoNatTypeEqual' :: forall a b. PeanoNatType a -> 'Succ (Add a b) :~: Add a ('Succ b)
succAddPeanoNatTypeEqual' ZeroType = Refl
succAddPeanoNatTypeEqual' (SuccType a) =
    case succAddPeanoNatTypeEqual' @_ @b a of
        Refl -> Refl

succAddPeanoNatTypeEqual :: forall a b. PeanoNatType a -> PeanoNatType b -> 'Succ (Add a b) :~: Add a ('Succ b)
succAddPeanoNatTypeEqual a _ = succAddPeanoNatTypeEqual' @a @b a

addPeanoNatTypeGE :: PeanoNatType a -> PeanoNatType b -> GreaterEqual (Add a b) a
addPeanoNatTypeGE a ZeroType =
    case addZeroPeanoNatTypeEqual a of
        Refl -> samePeanoGreaterEqual a
addPeanoNatTypeGE a (SuccType b) =
    case succAddPeanoNatTypeEqual a b of
        Refl -> diff1GreaterEqual $ addPeanoNatTypeGE a b

type Greater :: PeanoNat -> PeanoNat -> Type
type role Greater nominal nominal
data Greater a b where
    MkGreater :: GreaterEqual a b -> Greater ('Succ a) b

greaterIndex :: Greater a b -> PeanoNatType b
greaterIndex (MkGreater n) = greaterEqualIndex n

peanoGreater :: PeanoNatType a -> PeanoNatType b -> Maybe (Greater a b)
peanoGreater (SuccType a) b = fmap MkGreater $ peanoGreaterEqual a b
peanoGreater ZeroType _ = Nothing

instance Eq (Greater 'Zero b) where
    (==) = never

instance Searchable (Greater 'Zero b) where
    search = finiteSearch

instance Countable (Greater 'Zero b) where
    countPrevious = never
    countMaybeNext Nothing = Nothing
    countMaybeNext (Just n) = never n

instance Finite (Greater 'Zero b) where
    allValues = []
    assemble _ = pure never

instance Empty (Greater 'Zero b) where
    never n = case n of {}
