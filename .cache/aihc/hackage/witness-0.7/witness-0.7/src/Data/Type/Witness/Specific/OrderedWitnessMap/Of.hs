module Data.Type.Witness.Specific.OrderedWitnessMap.Of where

import Data.Type.Witness.General.Order
import Data.Type.Witness.Specific.OrderedWitnessMap.For
import Data.Type.Witness.Specific.Some
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type OrderedWitnessMapOf :: (Type -> Type) -> Type
type OrderedWitnessMapOf = OrderedWitnessMapFor Identity

-- | An empty dictionary.
emptyOrderedWitnessMapOf :: TestOrder w => OrderedWitnessMapOf w
emptyOrderedWitnessMapOf = emptyOrderedWitnessMapFor

-- | Look up the first value in the dictionary that matches the given witness.
orderedWitnessMapOfLookup :: TestOrder w => w a -> OrderedWitnessMapOf w -> Maybe a
orderedWitnessMapOfLookup wit d = fmap runIdentity $ orderedWitnessMapForLookup wit d

-- | Modify the first value in the dictionary that matches a particular witness.
orderedWitnessMapOfModify :: TestOrder w => w a -> (a -> a) -> OrderedWitnessMapOf w -> OrderedWitnessMapOf w
orderedWitnessMapOfModify wit amap = orderedWitnessMapForModify wit (Identity . amap . runIdentity)

-- | Replace the first value in the dictionary that matches the witness
orderedWitnessMapOfReplace :: TestOrder w => w a -> a -> OrderedWitnessMapOf w -> OrderedWitnessMapOf w
orderedWitnessMapOfReplace wit newa = orderedWitnessMapOfModify wit $ const newa

orderedWitnessMapOfSingle :: w a -> a -> OrderedWitnessMapOf w
orderedWitnessMapOfSingle wit a = orderedWitnessMapForSingle wit $ Identity a

orderedWitnessMapOfFold :: Monoid m => OrderedWitnessMapOf w -> (forall a. w a -> a -> m) -> m
orderedWitnessMapOfFold d f = orderedWitnessMapForFold d $ \wa (Identity a) -> f wa a

-- | Add a witness and value as the first entry in the dictionary.
orderedWitnessMapOfAdd :: TestOrder w => w a -> a -> OrderedWitnessMapOf w -> OrderedWitnessMapOf w
orderedWitnessMapOfAdd wit a = orderedWitnessMapForAdd wit $ Identity a

-- | Remove the first entry in the dictionary that matches the given witness.
orderedWitnessMapOfRemove :: TestOrder w => w a -> OrderedWitnessMapOf w -> OrderedWitnessMapOf w
orderedWitnessMapOfRemove = orderedWitnessMapForRemove

-- | Create a dictionary from a list of witness\/value pairs
orderedWitnessMapOfFromList :: TestOrder w => [SomeOf w] -> OrderedWitnessMapOf w
orderedWitnessMapOfFromList = orderedWitnessMapForFromList

orderedWitnessMapOfToList :: OrderedWitnessMapOf w -> [SomeOf w]
orderedWitnessMapOfToList = orderedWitnessMapForToList
