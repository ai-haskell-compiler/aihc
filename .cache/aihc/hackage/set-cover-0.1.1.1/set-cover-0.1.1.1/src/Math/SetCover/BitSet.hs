module Math.SetCover.BitSet where

import qualified Math.SetCover.Bit as Bit
import Math.SetCover.Bit ((.|.), (.&.))

import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))


newtype Set bits = Set {getBits :: bits} deriving (Eq, Ord, Show)

instance (Bit.C bits) => Semigroup (Set bits) where
   Set x <> Set y = Set $ x.|.y

instance (Bit.C bits) => Monoid (Set bits) where
   mempty = empty
   mappend = (<>)

empty :: Bit.C bits => Set bits
empty = Set Bit.empty

null :: Bit.C bits => Set bits -> Bool
null (Set xs)  =  xs == Bit.empty

keepMinimum :: Bit.C bits => Set bits -> Set bits
keepMinimum (Set xs) = Set $ Bit.keepMinimum xs

disjoint :: Bit.C bits => Set bits -> Set bits -> Bool
disjoint (Set xs) (Set ys)  =  xs.&.ys == Bit.empty

difference :: Bit.C bits => Set bits -> Set bits -> Set bits
difference (Set xs) (Set ys) = Set $ Bit.difference xs ys
