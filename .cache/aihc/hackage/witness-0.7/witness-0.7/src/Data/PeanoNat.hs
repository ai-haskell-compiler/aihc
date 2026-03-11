module Data.PeanoNat where

import GHC.TypeNats
import Import

-- | Inductive natural numbers.
data PeanoNat
    = Zero
    | Succ PeanoNat

addPeanoNat :: PeanoNat -> PeanoNat -> PeanoNat
addPeanoNat Zero b = b
addPeanoNat (Succ a) b = Succ $ addPeanoNat a b

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add a b where
    Add 'Zero b = b
    Add ('Succ a) b = 'Succ (Add a b)

-- | subtractFromPeanoNat a b = b - a
subtractFromPeanoNat :: PeanoNat -> PeanoNat -> Maybe PeanoNat
subtractFromPeanoNat Zero b = Just b
subtractFromPeanoNat (Succ a) (Succ b) = subtractFromPeanoNat a b
subtractFromPeanoNat (Succ _) Zero = Nothing

multiplyPeanoNat :: PeanoNat -> PeanoNat -> PeanoNat
multiplyPeanoNat Zero _ = Zero
multiplyPeanoNat (Succ a) b = addPeanoNat (multiplyPeanoNat a b) b

peanoToNatural :: PeanoNat -> Natural
peanoToNatural Zero = 0
peanoToNatural (Succ n) = succ $ peanoToNatural n

naturalToPeano :: Natural -> PeanoNat
naturalToPeano 0 = Zero
naturalToPeano n = Succ $ naturalToPeano $ pred n

type PeanoToNatural :: PeanoNat -> Nat
type family PeanoToNatural pn where
    PeanoToNatural 'Zero = 0
    PeanoToNatural ('Succ pn) = PeanoToNatural pn + 1

type ListLength :: forall k. [k] -> PeanoNat
type family ListLength l where
    ListLength '[] = 'Zero
    ListLength (_ ': aa) = 'Succ (ListLength aa)
