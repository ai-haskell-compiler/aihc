module Data.Type.Witness.Specific.WitnessMap.Of where

import Data.Type.Witness.Specific.Some
import Data.Type.Witness.Specific.WitnessMap.For
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessMapOf :: (Type -> Type) -> Type
type WitnessMapOf = WitnessMapFor Identity

-- | An empty dictionary.
emptyWitnessMapOf :: WitnessMapOf w
emptyWitnessMapOf = emptyWitnessMapFor

-- | Look up the first value in the dictionary that matches the given witness.
witnessMapOfLookup :: TestEquality w => w a -> WitnessMapOf w -> Maybe a
witnessMapOfLookup wit d = fmap runIdentity $ witnessMapForLookup wit d

-- | Modify the first value in the dictionary that matches a particular witness.
witnessMapOfModify :: TestEquality w => w a -> (a -> a) -> WitnessMapOf w -> WitnessMapOf w
witnessMapOfModify wit amap = witnessMapForModify wit (Identity . amap . runIdentity)

-- | Replace the first value in the dictionary that matches the witness
witnessMapOfReplace :: TestEquality w => w a -> a -> WitnessMapOf w -> WitnessMapOf w
witnessMapOfReplace wit newa = witnessMapOfModify wit $ const newa

witnessMapOfSingle :: w a -> a -> WitnessMapOf w
witnessMapOfSingle wit a = witnessMapForSingle wit $ Identity a

witnessMapOfFold :: Monoid m => WitnessMapOf w -> (forall a. w a -> a -> m) -> m
witnessMapOfFold d f = witnessMapForFold d $ \wa (Identity a) -> f wa a

-- | Add a witness and value as the first entry in the dictionary.
witnessMapOfAdd :: w a -> a -> WitnessMapOf w -> WitnessMapOf w
witnessMapOfAdd wit a = witnessMapForAdd wit $ Identity a

-- | Remove the first entry in the dictionary that matches the given witness.
witnessMapOfRemove :: TestEquality w => w a -> WitnessMapOf w -> WitnessMapOf w
witnessMapOfRemove = witnessMapForRemove

-- | Create a dictionary from a list of witness\/value pairs
witnessMapOfFromList :: [SomeOf w] -> WitnessMapOf w
witnessMapOfFromList = MkWitnessMapFor

witnessMapOfToList :: WitnessMapOf w -> [SomeOf w]
witnessMapOfToList = witnessMapForToList
