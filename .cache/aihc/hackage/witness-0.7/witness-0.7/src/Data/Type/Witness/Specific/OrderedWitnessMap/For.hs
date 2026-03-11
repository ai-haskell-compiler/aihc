module Data.Type.Witness.Specific.OrderedWitnessMap.For where

import Data.Map qualified as Map
import Data.Type.Witness.General.Order
import Data.Type.Witness.Specific.Some
import Import

-- | A dictionary that is heterogenous up to its simple witness type @w@.
-- Witnesses are the keys of the dictionary, and the values they witness are the values of the dictionary.
type OrderedWitnessMapFor :: forall k. (k -> Type) -> (k -> Type) -> Type
type role OrderedWitnessMapFor representational nominal
newtype OrderedWitnessMapFor f w =
    MkOrderedWitnessMapFor (Map.Map (Some w) (SomeFor f w))
    deriving newtype (Semigroup, Monoid)

-- | An empty dictionary.
emptyOrderedWitnessMapFor :: TestOrder w => OrderedWitnessMapFor f w
emptyOrderedWitnessMapFor = mempty

-- | Look up the value in the dictionary that matches the given witness.
orderedWitnessMapForLookup :: TestOrder w => w a -> OrderedWitnessMapFor f w -> Maybe (f a)
orderedWitnessMapForLookup wit (MkOrderedWitnessMapFor wmap) = do
    MkSomeFor wa fa <- Map.lookup (MkSome wit) wmap
    Refl <- testEquality wa wit -- should always succeed
    return fa

-- | Modify the value in the dictionary that matches a particular witness.
orderedWitnessMapForModify ::
       forall f w a. TestOrder w
    => w a
    -> (f a -> f a)
    -> OrderedWitnessMapFor f w
    -> OrderedWitnessMapFor f w
orderedWitnessMapForModify wit amap (MkOrderedWitnessMapFor wmap) = let
    updater :: SomeFor f w -> Maybe (SomeFor f w)
    updater (MkSomeFor wa fa) = do
        Refl <- testEquality wa wit -- should always succeed
        return $ MkSomeFor wa $ amap fa
    in MkOrderedWitnessMapFor $ Map.update updater (MkSome wit) wmap

-- | Replace the value in the dictionary that matches the witness
orderedWitnessMapForReplace :: TestOrder w => w a -> f a -> OrderedWitnessMapFor f w -> OrderedWitnessMapFor f w
orderedWitnessMapForReplace wit newfa = orderedWitnessMapForModify wit (const newfa)

-- | Add a witness and value to the dictionary.
orderedWitnessMapForAdd :: TestOrder w => w a -> f a -> OrderedWitnessMapFor f w -> OrderedWitnessMapFor f w
orderedWitnessMapForAdd wit fa (MkOrderedWitnessMapFor wmap) =
    MkOrderedWitnessMapFor $ Map.insert (MkSome wit) (MkSomeFor wit fa) wmap

-- | A dictionary for a single witness and value
orderedWitnessMapForSingle :: w a -> f a -> OrderedWitnessMapFor f w
orderedWitnessMapForSingle wit fa = MkOrderedWitnessMapFor $ Map.singleton (MkSome wit) $ MkSomeFor wit fa

orderedWitnessMapForFold :: Monoid m => OrderedWitnessMapFor f w -> (forall a. w a -> f a -> m) -> m
orderedWitnessMapForFold wmf f = mconcat $ fmap (\(MkSomeFor wa fa) -> f wa fa) $ orderedWitnessMapForToList wmf

-- | Remove the entry in the dictionary that matches the given witness.
orderedWitnessMapForRemove :: TestOrder w => w a -> OrderedWitnessMapFor f w -> OrderedWitnessMapFor f w
orderedWitnessMapForRemove wit (MkOrderedWitnessMapFor wmap) = MkOrderedWitnessMapFor $ Map.delete (MkSome wit) wmap

orderedWitnessMapForToList :: OrderedWitnessMapFor f w -> [SomeFor f w]
orderedWitnessMapForToList (MkOrderedWitnessMapFor wmap) = Map.elems wmap

-- | Create a dictionary from a list of witness\/value pairs
orderedWitnessMapForFromList :: TestOrder w => [SomeFor f w] -> OrderedWitnessMapFor f w
orderedWitnessMapForFromList ee =
    MkOrderedWitnessMapFor $ Map.fromList $ fmap (\swf@(MkSomeFor wa _) -> (MkSome wa, swf)) ee

orderedWitnessMapForMapM ::
       Applicative m => (forall a. f a -> m (g a)) -> OrderedWitnessMapFor f w -> m (OrderedWitnessMapFor g w)
orderedWitnessMapForMapM fmg (MkOrderedWitnessMapFor cells) =
    fmap MkOrderedWitnessMapFor $ for cells $ \(MkSomeFor wit fa) -> fmap (MkSomeFor wit) $ fmg fa
