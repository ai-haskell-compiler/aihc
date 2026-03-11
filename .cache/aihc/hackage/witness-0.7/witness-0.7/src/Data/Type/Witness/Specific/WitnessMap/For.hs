module Data.Type.Witness.Specific.WitnessMap.For where

import Data.Type.Witness.Specific.Some
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type WitnessMapFor :: forall k. (k -> Type) -> (k -> Type) -> Type
type role WitnessMapFor representational representational
newtype WitnessMapFor f w = MkWitnessMapFor
    { witnessMapForToList :: [SomeFor f w]
    } deriving newtype (Semigroup, Monoid)

-- | An empty dictionary.
emptyWitnessMapFor :: WitnessMapFor f w
emptyWitnessMapFor = mempty

-- | Look up the first value in the dictionary that matches the given witness.
witnessMapForLookup :: TestEquality w => w a -> WitnessMapFor f w -> Maybe (f a)
witnessMapForLookup wit (MkWitnessMapFor cells) = listToMaybe (mapMaybe (matchSomeFor wit) cells)

-- | Modify the first value in the dictionary that matches a particular witness.
witnessMapForModify :: TestEquality w => w a -> (f a -> f a) -> WitnessMapFor f w -> WitnessMapFor f w
witnessMapForModify wit amap (MkWitnessMapFor cells) =
    MkWitnessMapFor (replaceFirst ((fmap ((MkSomeFor wit) . amap)) . (matchSomeFor wit)) cells)
  where
    replaceFirst :: (a -> Maybe a) -> [a] -> [a]
    replaceFirst ama (a:aa) =
        case ama a of
            Just newa -> (newa : aa)
            _ -> a : (replaceFirst ama aa)
    replaceFirst _ _ = []

-- | Replace the first value in the dictionary that matches the witness
witnessMapForReplace :: TestEquality w => w a -> f a -> WitnessMapFor f w -> WitnessMapFor f w
witnessMapForReplace wit newfa = witnessMapForModify wit (const newfa)

-- | Add a witness and value as the first entry in the dictionary.
witnessMapForAdd :: w a -> f a -> WitnessMapFor f w -> WitnessMapFor f w
witnessMapForAdd wit fa (MkWitnessMapFor cells) = MkWitnessMapFor ((MkSomeFor wit fa) : cells)

-- | A dictionary for a single witness and value
witnessMapForSingle :: w a -> f a -> WitnessMapFor f w
witnessMapForSingle wit fa = MkWitnessMapFor $ pure $ MkSomeFor wit fa

witnessMapForFold :: Monoid m => WitnessMapFor f w -> (forall a. w a -> f a -> m) -> m
witnessMapForFold (MkWitnessMapFor cells) f = mconcat $ fmap (\(MkSomeFor wit fa) -> f wit fa) cells

-- | Remove the first entry in the dictionary that matches the given witness.
witnessMapForRemove :: TestEquality w => w a -> WitnessMapFor f w -> WitnessMapFor f w
witnessMapForRemove wit (MkWitnessMapFor cells) =
    MkWitnessMapFor (removeFirst (\(MkSomeFor cwit _) -> isJust (testEquality wit cwit)) cells)
  where
    removeFirst :: (a -> Bool) -> [a] -> [a]
    removeFirst p (a:as)
        | p a = as
    removeFirst p (a:as) = a : (removeFirst p as)
    removeFirst _ _ = []

-- | Create a dictionary from a list of witness\/value pairs
witnessMapForFromList :: [SomeFor f w] -> WitnessMapFor f w
witnessMapForFromList = MkWitnessMapFor

witnessMapForMapM :: Applicative m => (forall a. f a -> m (g a)) -> WitnessMapFor f w -> m (WitnessMapFor g w)
witnessMapForMapM fmg (MkWitnessMapFor cells) =
    fmap MkWitnessMapFor $ for cells $ \(MkSomeFor wit fa) -> fmap (MkSomeFor wit) $ fmg fa
